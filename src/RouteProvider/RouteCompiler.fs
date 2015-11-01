module RouteCompiler
open FParsec
open Route
open System.Text

type RouteProviderOptions =
   { typeName: string
     routesStr: string}

type Klass = 
  { name: string
    ctor: HandlerCtorParam list option
    subClasses: Klass list
    instanceVariables: InstanceVariable list
    methods: Method list }
  static member Empty () =
    { name = ""; ctor = None; subClasses = []; instanceVariables = []; methods = [] }
and DynamicParam = 
  | Int64Param of string
and InstanceVariable = | InstanceVariable of  DynamicParam
and FunctionParam = | FunctionParam of DynamicParam
and HandlerCtorParam =
  { name: string
    handlerArgs: FunctionParam list}
and Method =
  { name: string
    functionParams: FunctionParam list}

let routeName (route:Route) =
  let routeParts = List.map  (function | Constant(name) | NumericID(name) -> name) (route.routeSegments)
  sprintf "%s__%s" (route.verb) (String.concat "__" routeParts)

let routeIVars (route:Route) =
  List.choose
    (function
      | Constant(_) -> None
      | NumericID(name) -> Some(InstanceVariable(Int64Param(name)))) 
    (route.routeSegments)

let routeSubClass (route:Route) =
  { Klass.Empty() with 
      name = (routeName route) 
      instanceVariables = (routeIVars route) }
 
let makeSubClasses (routes:Route list) =
  List.map routeSubClass routes

let handlerName (route:Route) =
  sprintf "%s_Handler" (routeName route)

let makeHandlerCtorParam (route:Route) =
  { name = (handlerName route)
    handlerArgs = (List.choose
                    (function
                      | NumericID(name) -> 
                        Some(FunctionParam(Int64Param(name)))
                      | _ -> None)
                    (route.routeSegments)) }

let makeCtor (routes:Route list) = 
  Some((routes |> List.map makeHandlerCtorParam))

let routes2Class (routes:Route list) (typeName: string) =
  { name = typeName
    ctor = (makeCtor routes)
    subClasses = (makeSubClasses routes)
    instanceVariables = []
    methods = [] }

type ClassWriter () =
  let mutable indentation = 0
  member val content = new StringBuilder() with get
  member x.indent () = 
    indentation <- indentation + 2
    { new System.IDisposable with 
        member x.Dispose() = 
          indentation <- indentation - 2 }
  member w.block () = 
    w.Write("{\n")
    indentation <- indentation + 2
    { new System.IDisposable with 
        member x.Dispose() = 
          indentation <- indentation - 2
          w.WriteLine("}") }
  member x.WriteLine(line: string) = 
    let pfx = new string(' ', indentation)
    let s = sprintf "%s%s\n" pfx line
    x.content.Append(s) |> ignore
  member x.WriteFragment(str:string) =
    let pfx = new string(' ', indentation)
    let s = sprintf "%s%s" pfx str
    x.content.Append(s) |> ignore
  member x.Write(str:string) =
    x.content.Append(str) |> ignore

let renderSubClass (klass:Klass) (w:ClassWriter) =
  w.WriteFragment <| sprintf "public class %s " (klass.name)
  using (w.block()) (fun _ ->
    for ivar in klass.instanceVariables do
      match ivar with
      | InstanceVariable(Int64Param(name)) ->
        w.WriteLine <| sprintf "public readonly long %s;" name
      //| _ -> failwith "Ivar not implemented"
    )
  

let paramListTypeString (paramList: DynamicParam list) =
  let genericTypes = 
    paramList
    |> List.map (function | Int64Param(_) -> "long")

  if genericTypes.Length > 0 then
    sprintf "Action<%s>" (String.concat ", " genericTypes)
  else
    "Action"

let renderMainCtor (klassName:string) (ctorParams:HandlerCtorParam list) (w:ClassWriter) =
  w.WriteLine <| sprintf "public %s(" klassName

  using (w.indent()) (fun _ ->
      let ctorParams = ctorParams |> Array.ofList
      for i in [0..ctorParams.Length - 1] do
        let ctorParam = ctorParams.[i]
        let ctorArgs = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)
        w.WriteFragment <| sprintf "%s %s" (paramListTypeString ctorArgs) (ctorParam.name)
        if i <> ctorParams.Length - 1 then
          w.Write(",\n")  
      
      w.Write(") ")
      using (w.block()) (fun _ ->
        for ctorParam in ctorParams do
            w.WriteLine <| (sprintf "this.%s = %s;" (ctorParam.name) (ctorParam.name))
      )
  )

let renderHandlerCtorParamsIvars (ctorParams:HandlerCtorParam list) (w:ClassWriter) =
  for ctorParam in ctorParams do
    let pms = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)
    w.WriteLine <| (sprintf "public readonly %s %s;" (paramListTypeString pms) (ctorParam.name))

type RouteNode = 
  { endPoints: Endpoint list
    children: (NamedRouteSegment * RouteNode) list
    depth: int }
  static member Empty () = 
    { endPoints = []
      children = []
      depth = 0 }
and Endpoint = 
  { verb: string; handlerName: string }

let rec buildNode (route:Route) (segsRemaining: NamedRouteSegment list) (depth: int) =
  match segsRemaining with
  | [] -> 
    let endPoint = { verb = route.verb; handlerName = (handlerName route)}
    { endPoints = [endPoint]; children = []; depth = depth }
  | seg::segs ->
    let child = seg, (buildNode route segs (depth + 1))
    { endPoints = []; children = [child]; depth = depth }

let rec addRoute (routeNode:RouteNode) (route:Route) (segsRemaining: NamedRouteSegment list) (depth: int) =
  match segsRemaining with
  | [] ->
    let endPoint = { verb = route.verb; handlerName = (handlerName route)}
    { routeNode with endPoints = routeNode.endPoints @ [endPoint] }
  | seg::segs ->
    let existingChild = 
      routeNode.children 
      |> Seq.tryFind (fun (s, _) -> s = seg)
    match existingChild with
    | None -> 
      let newChild = seg, buildNode route segs (depth + 1)
      { routeNode with children = routeNode.children @ [newChild]}
    | Some(existingChild) ->
      let updatedChildren =
        routeNode.children
        |> List.map (fun child -> 
          if child = existingChild then
            let seg, routeNode = child
            (seg, (addRoute routeNode route segs (depth + 1)))
          else
            child) 
      { routeNode with children = updatedChildren }   

let buildRouteTree (routes: Route list) =
  let rec addRouteF routeNode routes =
    match routes with
    | route :: routes ->
      let updatedRouteNode = addRoute routeNode route (route.routeSegments) 0
      addRouteF updatedRouteNode routes
    | [] ->
      routeNode

  addRouteF (RouteNode.Empty()) routes

type FlatRouteMember =
  | NamedRouteSegment of NamedRouteSegment
  | Endpoint of Endpoint

let flattenRouteTree (routeTree:RouteNode) : FlatRouteMember list seq =
  let rec walkAux (routeTree:RouteNode) =
    seq {
      for e in routeTree.endPoints do
        yield [Endpoint(e)]

      for seg, child in routeTree.children do
        yield! (walkAux child) |> Seq.map (fun n -> NamedRouteSegment(seg) :: n)
    }
  walkAux routeTree 

type RouteTreeFragment = 
  { head: FlatRouteMember
    children : RouteTreeFragment list}

let groupRoutes (flatRoutes: FlatRouteMember list seq) : (int * RouteTreeFragment list) list =
  let rec addFlatRoute (memo: RouteTreeFragment list) (flatroute:FlatRouteMember list)  =
    match flatroute with
    | [] -> memo
    | seg::segs ->
      match memo |> List.tryFind (fun t -> t.head = seg) with
      | Some(t) ->
        let newChildren = addFlatRoute (t.children) segs
        memo |> List.map (fun t2 -> 
          if t2 = t then
            { t with children = newChildren }
          else
            t2)
      | None ->
        let t = { head = seg; children = (addFlatRoute [] segs)}
        t :: memo
      
  flatRoutes 
  |> Seq.groupBy (fun s -> Seq.length s - 1)
  |> Seq.map (fun (n, rs) -> n, (rs |> Seq.fold addFlatRoute []) )
  |> List.ofSeq

type RouteIfTest = 
  { partIdx: int option
    seg: FlatRouteMember
    children: RouteIfTest list }

let rec route2If (route: RouteTreeFragment) (idx:int) : RouteIfTest =
  match route.head with
    | NamedRouteSegment(_) as seg ->
      match route.children with
      | [] ->
        { partIdx = Some(idx); seg = seg; children = [] }
      | children ->
        let childIfs = children |> List.map (fun c -> route2If c (idx + 1))
        { partIdx = Some(idx); seg = seg; children = childIfs }
    | Endpoint(_) as endpoint ->
      // Don't need to worry about children here, because it isn't possible with the way we group routes
      { partIdx = None; seg = endpoint; children = [] }

let renderIf (routeIf:RouteIfTest) (w:ClassWriter) = 
  let rec renderIfAux (routeIf:RouteIfTest) (precedingSegs:NamedRouteSegment list) (first:bool) (depth:int) =
    let keyword = if first then "if" else "else if"    
    match routeIf.seg with
    | NamedRouteSegment(seg) ->
      let idx = routeIf.partIdx.Value
      match seg with 
      | Constant(name) ->
        w.WriteFragment <| sprintf "%s (parts[%d] == \"%s\")" keyword idx name
        using (w.block()) (fun _ ->
          routeIf.children |> List.iteri (fun i child ->
            renderIfAux child (seg :: precedingSegs) (i = 0) (depth + 1)))
      | NumericID(name) ->
        w.WriteFragment <| sprintf "%s (StringIsAllDigits(parts[%d]))" keyword idx
        using (w.block()) (fun _ ->
          w.WriteLine <| sprintf "var %s = long.Parse(parts[%d]);" name idx
          routeIf.children |> List.iteri (fun i child ->
            renderIfAux child (seg :: precedingSegs) (i = 0) (depth + 1)))
      if depth = 0 then w.WriteLine("break;")
    | Endpoint(endpoint) ->
      let args = 
        precedingSegs 
        |> List.rev 
        |> List.choose (function
          | NumericID(n) -> Some(n) 
          | _ -> None)
        |> String.concat ", "
      w.WriteLine <| sprintf "%s (verb == \"%s\") { this.%s(%s); return; }" keyword (endpoint.verb) (endpoint.handlerName) args

  renderIfAux routeIf [] true 0

let renderRouteGroupMatchTest (group:RouteTreeFragment list) (w:ClassWriter) =
  for route in group do
    let IF = route2If route 0
    renderIf IF w

let renderDispatchMethod (routeTree:RouteNode) (w:ClassWriter) =
  let routeGroups = routeTree |> flattenRouteTree |> groupRoutes
  System.Diagnostics.Debug.Print <| sprintf "routeGroups:\n\n%A" routeGroups
  w.WriteFragment("public void DispatchRoute(string verb, string path) ")

  using (w.block()) (fun _ ->
    w.WriteLine <| "var parts = path.Split('/');"
    w.WriteFragment("switch (parts.Length) ")
    using (w.block()) (fun _ ->
      for n, group in routeGroups do
        w.WriteLine <| sprintf "case %d:" n
        using (w.indent()) (fun _ ->
          renderRouteGroupMatchTest group w)
      w.WriteLine <| "default: throw new ArgumentException();" // Todo: replace with routenotfoundexception
    ))

let renderDigitCheckFn (w:ClassWriter) = 
  w.WriteFragment <| "static bool StringIsAllDigits(string s)"
  using (w.block()) (fun _ ->
    w.WriteFragment <| "foreach (char c in s)"
    using (w.block()) (fun _ ->
      w.WriteLine <| "if (c < '0' || c > '9') { return false; }")
    w.WriteLine <| "return true;")

let renderMainClass (klass:Klass) (routeTree:RouteNode) =
  let w = new ClassWriter()
  w.WriteLine "using System;"
  w.WriteFragment "namespace IsakSky "
  using (w.block()) (fun _ ->
    w.WriteFragment <| sprintf "public class %s " (klass.name)
    using (w.block()) (fun _ ->
      renderDigitCheckFn w

      for k in klass.subClasses do renderSubClass k w

      match klass.ctor with
      | Some(ctorParams) ->
        renderMainCtor (klass.name) ctorParams w
        renderHandlerCtorParamsIvars ctorParams w
      | None -> failwith "Missing ctor"

      System.Diagnostics.Debug.Print <| sprintf "RouteTree:\n\n%A" routeTree

      renderDispatchMethod routeTree w

    )
  )
  let code = w.content.ToString()
  System.Diagnostics.Debug.Print(code)
  code

let compileRoutes (options:RouteProviderOptions) =
  match runParserOnString RouteParsing.pRoutes () "User routes" (options.routesStr) with
  | Success(routes,_, _) ->
    let klass = routes2Class routes (options.typeName)
    let routeTree = buildRouteTree routes
    let code = renderMainClass klass routeTree
    let dllFile = System.IO.Path.GetTempFileName()
    let compiler = new Microsoft.CSharp.CSharpCodeProvider()
    let parameters = new System.CodeDom.Compiler.CompilerParameters()
    parameters.OutputAssembly <- dllFile
    parameters.CompilerOptions <- "/t:library"
    let compilerResults = compiler.CompileAssemblyFromSource(parameters, [| code |])
    let asm = compilerResults.CompiledAssembly
    asm.GetType <| "IsakSky." + options.typeName
  | Failure (msg,_,_) ->
    failwithf "Failed to parse routes. Error: %s" msg