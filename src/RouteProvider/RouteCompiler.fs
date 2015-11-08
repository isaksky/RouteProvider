module RouteCompiler
open FParsec
open Route
open System.Text

type RouteProviderOptions =
   { typeName: string
     routesStr: string }

type RouterKlass = 
  { name: string
    ctor: HandlerCtorParam list option
    routeKlasses: RouteBuilder list
    methods: Method list }
  static member Empty () =
    { name = ""; ctor = None; routeKlasses = []; methods = [] }
and RouteBuilder = 
  { name: string
    arguments: FunctionParam list
    segments: NamedRouteSegment list }
and DynamicParam = 
  | Int64Param of string
and FunctionParam = | FunctionParam of DynamicParam
and HandlerCtorParam =
  { name: string
    handlerArgs: FunctionParam list }
and Method =
  { name: string
    functionParams: FunctionParam list }

let routeName (route:Route) =
  match route.routeName with
  | Some(routeName) ->
    routeName
  | None ->
    let routeParts = route.routeSegments |> List.choose  (function 
      | Constant(name) -> Some(name)
      | NumericID(_) -> None) 
    sprintf "%s__%s" (route.verb) (String.concat "_" routeParts)

let routeIVars (route:Route) =
  List.choose
    (function
      | Constant(_) -> None
      | NumericID(name) -> Some(FunctionParam(Int64Param(name)))) 
    (route.routeSegments)

let routeSubClass (route:Route) =
  { name = (routeName route) 
    arguments = (routeIVars route)
    segments = route.routeSegments }
 
let makeSubClasses (routes:Route list) =
  List.map routeSubClass routes

let handlerName (route:Route) =
  sprintf "%sHandler" (routeName route)

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
    routeKlasses = (makeSubClasses routes)
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
          w.StartWriteLine("}") }
  member x.StartWriteLine(line: string) = 
    let pfx = new string(' ', indentation)
    let s = sprintf "%s%s\n" pfx line
    x.content.Append(s) |> ignore
  member x.StartWrite(str:string) =
    let pfx = new string(' ', indentation)
    let s = sprintf "%s%s" pfx str
    x.content.Append(s) |> ignore
  member x.Write(str:string) =
    x.content.Append(str) |> ignore

let renderRouteBuilder (klass:RouteBuilder) (w:ClassWriter) =
  w.StartWrite <| sprintf "public static string %s(" (klass.name)
  
  let argStrs =
    klass.arguments
    |> List.map (function
      | FunctionParam(Int64Param(name)) ->
        sprintf "long %s" name)

  w.Write <| String.concat ", " argStrs
  w.Write ")"

  using (w.block()) (fun _ ->
    let hasDynSeg = klass.segments |> List.tryFind (function | NumericID(_) -> true | _ -> false) |> Option.isSome
    if hasDynSeg then
      let rec buildFmtStr segs dynIdx ret =
        match segs with
        | [] -> ret
        | Constant(name)::segs ->
          buildFmtStr segs dynIdx (name::ret)
        | NumericID(_)::segs ->
          let s = sprintf "{%d}" dynIdx
          buildFmtStr segs (dynIdx + 1) (s::ret)
            
      let fmtStr = 
        buildFmtStr klass.segments 0 []
        |> List.rev          
        |> String.concat "/"
        |> fun s -> "/" + s

      let argsStr = 
        klass.segments 
          |> List.choose (fun seg ->
            match seg with
            | Constant(_) -> None
            | NumericID(name) -> Some(name))
          |> String.concat ", "

      w.StartWriteLine <| sprintf "return string.Format(\"%s\", %s);" fmtStr argsStr
    else
      let constStr =
          klass.segments 
          |> List.map (fun seg ->
            match seg with
            | Constant(name) -> name
            | _ -> failwith "Logic error")
          |> String.concat "/"
      w.StartWriteLine <| sprintf "return \"/%s\";" constStr)

let paramListTypeString (paramList: DynamicParam list) =
  let genericTypes = 
    paramList
    |> List.map (function | Int64Param(_) -> "long")

  if genericTypes.Length > 0 then
    sprintf "Action<%s>" (String.concat ", " genericTypes)
  else
    "Action"

let renderMainCtor (klassName:string) (ctorParams:HandlerCtorParam list) (w:ClassWriter) =
  w.StartWriteLine <| sprintf "public %s(" klassName

  using (w.indent()) (fun _ ->
      let ctorParams = ctorParams |> Array.ofList
      for i in [0..ctorParams.Length - 1] do
        let ctorParam = ctorParams.[i]
        let ctorArgs = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)
        w.StartWrite <| sprintf "%s %s" (paramListTypeString ctorArgs) (ctorParam.name)
        if i <> ctorParams.Length - 1 then
          w.Write(",\n")  
      
      w.Write(") ")
      using (w.block()) (fun _ ->
        for ctorParam in ctorParams do
            w.StartWriteLine <| (sprintf "this.%s = %s;" (ctorParam.name) (ctorParam.name))))

let renderHandlerCtorParamsIvars (ctorParams:HandlerCtorParam list) (w:ClassWriter) =
  for ctorParam in ctorParams do
    let pms = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)
    w.StartWriteLine <| (sprintf "public readonly %s %s;" (paramListTypeString pms) (ctorParam.name))

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
        w.StartWrite <| sprintf "%s (parts[start + %d] == \"%s\")" keyword idx name
        using (w.block()) (fun _ ->
          routeIf.children |> List.iteri (fun i child ->
            renderIfAux child (seg :: precedingSegs) (i = 0) (depth + 1)))
      | NumericID(name) ->
        w.StartWrite <| sprintf "%s (StringIsAllDigits(parts[start + %d]))" keyword idx
        using (w.block()) (fun _ ->
          w.StartWriteLine <| sprintf "var %s = long.Parse(parts[start + %d]);" name idx
          routeIf.children |> List.iteri (fun i child ->
            renderIfAux child (seg :: precedingSegs) (i = 0) (depth + 1)))
      if depth = 0 then w.StartWriteLine("break;")
    | Endpoint(endpoint) ->
      let args = 
        precedingSegs 
        |> List.rev 
        |> List.choose (function
          | NumericID(n) -> Some(n) 
          | _ -> None)
        |> String.concat ", "
      w.StartWriteLine <| sprintf "%s (verb == \"%s\") { this.%s(%s); return; }" keyword (endpoint.verb) (endpoint.handlerName) args

  renderIfAux routeIf [] true 0

let renderRouteGroupMatchTest (group:RouteTreeFragment list) (w:ClassWriter) =
  for route in group do
    let IF = route2If route 0
    renderIf IF w

let renderDispatchMethod (routeTree:RouteNode) (w:ClassWriter) =
  let routeGroups = routeTree |> flattenRouteTree |> groupRoutes
  System.Diagnostics.Debug.Print <| sprintf "routeGroups:\n\n%A" routeGroups
  w.StartWrite("public void DispatchRoute(string verb, string path) ")

  using (w.block()) (fun _ ->
    w.StartWriteLine <| "var parts = path.Split('/');"
    // Normalize starting and ending flash
    w.StartWriteLine <| "var start = 0;"
    w.StartWriteLine <| "if (parts[0] == \"\") { start = 1; }"
    w.StartWriteLine <| "var endOffset = parts.Length > 0 && parts[parts.Length - 1] == \"\" ? 1 : 0;"
    w.StartWrite <| "switch (parts.Length - start - endOffset) "
    using (w.block()) (fun _ ->
      for n, group in routeGroups do
        w.StartWriteLine <| sprintf "case %d:" n
        using (w.indent()) (fun _ ->
          renderRouteGroupMatchTest group w)
      w.StartWriteLine <| "default: break;")
    w.StartWriteLine <| "throw new RouteNotMatchedException(verb, path);")

let digitCheckFn = """
static bool StringIsAllDigits(string s) {
  foreach (char c in s) {
    if (c < '0' || c > '9') { return false; }
  }
  return true;
}"""

let routeNotMatchedEx = """
public class RouteNotMatchedException : Exception {
  public string Verb { get; private set; }
  public string Path { get; private set; }
  public RouteNotMatchedException(string verb, string path) {
    this.Verb = verb;
    this.Path = path;
  }
}"""
let renderMultiLineStr (w:ClassWriter) (s:string) =
  for line in s.Split('\n') do
    if not <| System.String.IsNullOrWhiteSpace(s) then
      w.StartWriteLine <| line

let renderUtilities (w:ClassWriter) = 
  renderMultiLineStr w digitCheckFn
  renderMultiLineStr w routeNotMatchedEx

let renderMainClass (klass:RouterKlass) (routeTree:RouteNode) =
  let w = new ClassWriter()
  w.StartWriteLine "using System;"
  w.StartWrite "namespace IsakSky "
  using (w.block()) (fun _ ->
    w.StartWrite <| sprintf "public class %s " (klass.name)
    using (w.block()) (fun _ ->
      w.StartWrite "public static class Builders"
      using (w.block()) (fun _ -> 
        for k in klass.routeKlasses do renderRouteBuilder k w 
      )
      match klass.ctor with
      | Some(ctorParams) ->
        renderMainCtor (klass.name) ctorParams w
        renderHandlerCtorParamsIvars ctorParams w
      | None -> failwith "Missing ctor"
      System.Diagnostics.Debug.Print <| sprintf "RouteTree:\n\n%A" routeTree
      w.Write("\n")
      renderDispatchMethod routeTree w
      renderUtilities w))
  w.content.ToString()

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