namespace IsakSky.RouteProvider
open FParsec
open Route
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO

module RouteCompiler =
  type RouteProviderOptions =
     { typeName: string
       routesStr: string
       inputTypeName: string option
       returnTypeName: string option
       config: TypeProviderConfig option }

  type RouterKlass = 
    { name: string
      ctor: HandlerCtorParam list option
      routeKlasses: RouteBuilder list
      methods: Method list }
  and RouteBuilder = 
    { name: string
      arguments: FunctionParam list
      segments: NamedRouteSegment list }
  and DynamicParam = 
    | Int64Param of string
    | IntParam of string
    | StringParam of string
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
        | ConstantSeg(name) -> Some(name)
        | _ -> None) 
      sprintf "%s__%s" (route.verb) (String.concat "_" routeParts)

  let routeIVars (route:Route) =
    List.choose
      (function
        | ConstantSeg(_) -> None
        | Int64Seg(name) -> Some(FunctionParam(Int64Param(name)))
        | IntSeg(name) -> Some(FunctionParam(IntParam(name)))
        | StringSeg(name) -> Some(FunctionParam(StringParam(name))))
      (route.routeSegments)

  let routeSubClass (route:Route) =
    { name = (routeName route) 
      arguments = (routeIVars route)
      segments = route.routeSegments }
 
  let makeRouteKlasses (routes:Route list) =
    List.map routeSubClass routes

  let handlerName = routeName 

  let makeHandlerCtorParam (route:Route) =
    { name = (handlerName route)
      handlerArgs = (List.choose
                      (function
                        | Int64Seg(name) -> Some(FunctionParam(Int64Param(name)))
                        | IntSeg(name) -> Some(FunctionParam(IntParam(name)))
                        | StringSeg(name) -> Some(FunctionParam(StringParam(name)))
                        | _ -> None)
                      (route.routeSegments)) }

  let makeCtor (routes:Route list) = 
    Some((routes |> List.map makeHandlerCtorParam))

  let routes2Class (routes:Route list) (options: RouteProviderOptions) =
    { name = options.typeName
      ctor = (makeCtor routes)
      routeKlasses = (makeRouteKlasses routes)
      methods = []}

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
        | FunctionParam(Int64Param(name)) -> sprintf "long %s" name
        | FunctionParam(IntParam(name)) -> sprintf "int %s" name
        | FunctionParam(StringParam(name)) -> sprintf "string %s" name)

    w.Write <| String.concat ", " argStrs
    w.Write ")"

    using (w.block()) (fun _ ->
      let hasDynSeg = klass.segments |> List.tryFind (function | ConstantSeg(_) -> false | _ -> true) |> Option.isSome
      if hasDynSeg then
        let rec buildFmtStr segs dynIdx ret =
          match segs with
          | [] -> ret
          | ConstantSeg(name)::segs ->
            buildFmtStr segs dynIdx (name::ret)
          | Int64Seg(_)::segs 
          | IntSeg(_)::segs 
          | StringSeg(_)::segs ->
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
              | ConstantSeg(_) -> None
              | Int64Seg(name) 
              | IntSeg(name) 
              | StringSeg(name) -> Some(name))
            |> String.concat ", "

        w.StartWriteLine <| sprintf "return string.Format(\"%s\", %s);" fmtStr argsStr
      else
        let constStr =
            klass.segments 
            |> List.map (fun seg ->
              match seg with
              | ConstantSeg(name) -> name
              | _ -> failwith "Logic error")
            |> String.concat "/"
        w.StartWriteLine <| sprintf "return \"/%s\";" constStr)

  let dynParamTypeName (dynParam:DynamicParam) =
    match dynParam with
    | Int64Param(_) -> "long"
    | IntParam(_) -> "int"
    | StringParam(_) -> "string"

  let paramListTypeString (paramList: DynamicParam list) (options:RouteProviderOptions) =
    let genericTypes = paramList |> List.map dynParamTypeName
    let ftype = if options.returnTypeName.IsSome then "Func" else "Action"

    let types' = 
      match options.inputTypeName with
      | Some(inputTypeName) ->
        inputTypeName :: genericTypes
      | None -> genericTypes

    let types =
      match options.returnTypeName with
      | Some(returnTypeName) ->
        types' @ [returnTypeName]
      | None -> types'

    if types.Length > 0 then
      sprintf "%s<%s>" ftype (String.concat ", " types)
    else
      ftype

  let renderMainCtor (klassName:string) (ctorParams:HandlerCtorParam list) (options:RouteProviderOptions) (w:ClassWriter) =
    w.StartWriteLine <| sprintf "public %s(" klassName

    using (w.indent()) (fun _ ->
        let ctorParams = ctorParams |> Array.ofList
        for i in [0..ctorParams.Length - 1] do
          let ctorParam = ctorParams.[i]
          let ctorArgs = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)
          w.StartWrite <| sprintf "%s %s" (paramListTypeString ctorArgs options) (ctorParam.name)
          if i <> ctorParams.Length - 1 then
            w.Write(",\n")  
      
        w.Write(") ")
        using (w.block()) (fun _ ->
          for ctorParam in ctorParams do
              w.StartWriteLine <| (sprintf "this.%s = %s;" (ctorParam.name) (ctorParam.name))))

  let renderHandlerCtorParamsIvars (ctorParams:HandlerCtorParam list) (options:RouteProviderOptions) (w:ClassWriter) =
    for ctorParam in ctorParams do
      let pms = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)
      w.StartWriteLine <| (sprintf "public readonly %s %s;" (paramListTypeString pms options) (ctorParam.name))

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

  type Mappings = 
    { int64s: (NamedRouteSegment * RouteNode) list
      ints: (NamedRouteSegment * RouteNode) list 
      strs: (NamedRouteSegment * RouteNode) list }
    static member Empty () =
      { int64s = []; ints = []; strs = [] }

  let addSeg mappings pair = 
    let seg, _ = pair
    match seg with
    | Int64Seg(_) ->
      { mappings with int64s = pair :: mappings.int64s}
    | IntSeg(_) -> 
      { mappings with ints = pair :: mappings.ints}
    | StringSeg(_) ->
      { mappings with strs = pair :: mappings.strs}
    | _ -> failwith "Logic error"

  let addMapping (m:Map<string, Mappings>) (k:string) (pair:NamedRouteSegment * RouteNode) : Map<string, Mappings> = 
    match Map.tryFind k m with
    | Some(mappings) ->
        Map.add k (addSeg mappings pair) m
    | None -> 
      let mappings = Mappings.Empty()
      Map.add k (addSeg mappings pair) m

  let flattenRouteTree (routeTree:RouteNode) : FlatRouteMember list seq =
    let rec walkAux (routeTree:RouteNode) depth =
      seq {
        for e in routeTree.endPoints do
          yield [Endpoint(e)]

        let rec group (children:(NamedRouteSegment * RouteNode) list) constants mappings =
          match children with
          | (seg, _) as c :: children -> 
            let constants, mappings = 
              match seg with
              | ConstantSeg(_) ->
                c :: constants, mappings
              | Int64Seg(name) | IntSeg(name) | StringSeg(name) ->
                let mappings = addMapping mappings name c
                constants, mappings
            group children constants mappings
          | [] -> 
            constants, mappings

        let constants, mappingsMap = group (routeTree.children) [] Map.empty

        for seg, child in constants do
          yield! (walkAux child (depth + 1)) |> Seq.map (fun n -> NamedRouteSegment(seg) :: n)

        for KeyValue(_, mappings) in mappingsMap do
          let nodes = 
            [mappings.int64s; mappings.ints; mappings.strs] 
            |> List.map List.rev 
            |> List.concat 
            |> Array.ofList
          for idx in [0..nodes.Length - 1] do
            let seg, child = nodes.[idx]
            let seg =
              match mappingsMap.Count with
              | 1 -> seg
              | _ ->
                match seg with
                | Int64Seg(_) -> Int64Seg(sprintf "int64Arg_%d_%d" depth idx)
                | IntSeg(_) -> IntSeg(sprintf "intArg%d_%d" depth idx)
                | StringSeg(_) -> StringSeg(sprintf "strArg_%d_%d" depth idx)
                | _ -> failwith "Logic error"

            yield! (walkAux child (depth + 1)) |> Seq.map (fun n -> NamedRouteSegment(seg) :: n) 
      }
    walkAux routeTree 0

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
          memo @ [t]
      
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

  let renderIf (routeIf:RouteIfTest) (options:RouteProviderOptions) (w:ClassWriter) = 
    let rec renderIfAux (routeIf:RouteIfTest) (precedingSegs:NamedRouteSegment list) (first:bool) (depth:int) =
      let keyword = if first then "if" else "else if"    
      match routeIf.seg with
      | NamedRouteSegment(seg) ->
        let idx = routeIf.partIdx.Value
        match seg with 
        | ConstantSeg(name) ->
          w.StartWrite <| sprintf "%s (parts[start + %d] == \"%s\")" keyword idx name
          using (w.block()) (fun _ ->
            routeIf.children |> List.iteri (fun i child ->
              renderIfAux child (seg :: precedingSegs) (i = 0) (depth + 1)))
        | StringSeg(name) ->
          if first then w.StartWrite("") else w.StartWrite "else"
          using (w.block()) (fun _ ->
            w.StartWriteLine <| sprintf "var %s = parts[start + %d];" name idx
            routeIf.children |> List.iteri (fun i child ->
              renderIfAux child (seg :: precedingSegs) (i = 0) (depth + 1)))
        | Int64Seg(name) 
        | IntSeg(name) ->
          let keyword = if first then "if" else "else if"    
          w.StartWrite <| sprintf "%s (StringIsAllDigits(parts[start + %d]))" keyword idx
          using (w.block()) (fun _ ->
            let tname = 
              match seg with
              | IntSeg(_) -> "int"
              | Int64Seg(_) -> "long"
              | _ -> failwith "Logic error"
            w.StartWriteLine <| sprintf "var %s = %s.Parse(parts[start + %d]);" name tname idx
            routeIf.children |> List.iteri (fun i child ->
              renderIfAux child (seg :: precedingSegs) (i = 0) (depth + 1)))
        //if depth = 0 then w.StartWriteLine("break;")
      | Endpoint(endpoint) ->
        let dynArgs = 
          precedingSegs 
          |> List.rev 
          |> List.choose (function
            | Int64Seg(n) | IntSeg(n) | StringSeg(n) -> Some(n) 
            | _ -> None)

        let args = 
          match options.inputTypeName with
          | Some(_) -> "context" :: dynArgs
          | None -> dynArgs

        let handlerCall =
          if options.returnTypeName.IsSome then
            sprintf "return this.%s(%s);" (endpoint.handlerName) (args |> String.concat ", ")
          else
            sprintf "this.%s(%s); return;" (endpoint.handlerName) (args |> String.concat ", ")
      
        w.StartWriteLine <| sprintf "%s (verb == \"%s\") { %s }" keyword (endpoint.verb) handlerCall

    renderIfAux routeIf [] true 0

  let renderRouteGroupMatchTest (group:RouteTreeFragment list) (options:RouteProviderOptions) (w:ClassWriter) =
    for route in group do
      let IF = route2If route 0
      Debug.Print <| sprintf "GROUP: %A" group
      renderIf IF options w

  let renderDispatchMethod (routeTree:RouteNode) (options:RouteProviderOptions) (w:ClassWriter) =
    let routeGroups = routeTree |> flattenRouteTree |> groupRoutes

    let baseArgs = [
      "string verb"
      "string path"  
    ]

    let args = 
      match options.inputTypeName with
      | Some(inputTypeName) ->
        sprintf "%s context" inputTypeName :: baseArgs
      | None -> baseArgs

    let retTp =
      match options.returnTypeName with
      | Some(routeTypeName) -> routeTypeName
      | None -> "void"
    
    let argsStr = args |> String.concat ", "
    w.StartWrite<| sprintf "public %s DispatchRoute(%s) " retTp argsStr
    using (w.block()) (fun _ ->
      w.StartWriteLine <| "var parts = path.Split('/');"
      // Normalize starting and ending slash
      w.StartWriteLine <| "var start = 0;"
      w.StartWriteLine <| "if (parts[0] == \"\") { start = 1; }"
      w.StartWriteLine <| "var endOffset = parts.Length > 0 && parts[parts.Length - 1] == \"\" ? 1 : 0;"
      w.StartWrite <| "switch (parts.Length - start - endOffset) "
      using (w.block()) (fun _ ->
        for n, group in routeGroups do
          w.StartWriteLine <| sprintf "case %d:" n
          using (w.indent()) (fun _ ->
            renderRouteGroupMatchTest group options w
            w.StartWriteLine "break;")
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
    let mutable startIdx = 0
    for line in s.Split('\n') do
      if not <| System.String.IsNullOrWhiteSpace(line) then
        if startIdx = 0 then startIdx <- line |> Seq.findIndex (fun c -> c <> ' ')
        w.StartWriteLine <| line.Substring(startIdx)

  let renderUtilities (w:ClassWriter) = 
    renderMultiLineStr w digitCheckFn
    renderMultiLineStr w routeNotMatchedEx

  let renderMainClass (klass:RouterKlass) (routeTree:RouteNode) (options:RouteProviderOptions) =
    let w = new ClassWriter()
    w.StartWriteLine "using System;"
    w.StartWrite "namespace IsakSky "
    using (w.block()) <| fun _ ->
      w.StartWrite <| sprintf "public class %s " (klass.name)
      using (w.block()) <| fun _ ->
        w.StartWrite "public static class Builders"
        using (w.block()) <| fun _ -> 
          for k in klass.routeKlasses do renderRouteBuilder k w 
      
        match klass.ctor with
        | Some(ctorParams) ->
          renderMainCtor (klass.name) ctorParams options w
          renderHandlerCtorParamsIvars ctorParams options w
        | None -> failwith "Missing ctor"
        System.Diagnostics.Debug.Print <| sprintf "RouteTree:\n\n%A" routeTree
        w.Write("\n")
        renderDispatchMethod routeTree options w
        renderUtilities w
    w.content.ToString()

  let buildCSharpCode (options:RouteProviderOptions) =
    match runParserOnString RouteParsing.pRoutes () "User routes" (options.routesStr) with
    | Success(routes,_, _) ->
      let klass = routes2Class routes options
      let routeTree = buildRouteTree routes
      renderMainClass klass routeTree options
    | Failure (msg,_,_) ->
      failwithf "Failed to parse routes. Error: %s" msg

  let compileRoutes (options:RouteProviderOptions) =
    match runParserOnString RouteParsing.pRoutes () "User routes" (options.routesStr) with
    | Success(routes,_, _) ->
      let klass = routes2Class routes options
      let routeTree = buildRouteTree routes
      let code = renderMainClass klass routeTree options
    
      let dllFile = System.IO.Path.GetTempFileName()
      let compilerArgs =  dict [("CompilerVersion", "v4.0")]
      let compiler = new Microsoft.CSharp.CSharpCodeProvider(compilerArgs)
    
      let parameters = new System.CodeDom.Compiler.CompilerParameters([|"mscorlib.dll"; "System.Core.dll"; "System.dll"|])
      parameters.TreatWarningsAsErrors <- true

      match options.config with
      | Some(config) ->
        for r in config.ReferencedAssemblies do
          if Path.GetFileName(r) <> "RouteProvider.dll" then
            Debug.Print <| sprintf "Added ref \"%s\"" r
            parameters.ReferencedAssemblies.Add(r) |> ignore
      | None -> ()

      parameters.OutputAssembly <- dllFile
      parameters.CompilerOptions <- "/t:library"
      let compilerResults = compiler.CompileAssemblyFromSource(parameters, [| code |])
      if compilerResults.Errors.HasErrors then
        let errors = seq { for err in compilerResults.Errors do yield err }
        failwithf "Got error: %A" (errors |> Seq.head)
      else
        let asm = System.Reflection.Assembly.LoadFrom(dllFile)
        asm.GetType <| "IsakSky." + options.typeName
    | Failure (msg,_,_) ->
      failwithf "Failed to parse routes. Error: %s" msg