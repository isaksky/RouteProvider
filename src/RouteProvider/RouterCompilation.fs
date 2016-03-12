namespace IsakSky.RouteProvider
open FParsec
open Route
open System
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

module RouteCompilation =
  type RouteCompilationArgs =
     { typeName: string
       parse: Route list
       inputType: bool
       returnType: bool
       nameSpace: string option
       moduleName: string option
       outputType: CompilationOutputType }
  and CompilationOutputType =
  | FSharp

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

  let routes2Class (routes:Route list) (options: RouteCompilationArgs) =
    { name = options.typeName
      ctor = (makeCtor routes)
      routeKlasses = (makeRouteKlasses routes)
      methods = []}

  type FSharpWriter (indentSize:int) =
    let mutable indentation = 0
    member val IndentSize = indentSize with get
    member val content = new StringBuilder() with get
    member x.IncIndent () =
      indentation <- indentation + x.IndentSize
    member x.DecIndent () =
      indentation <- indentation - x.IndentSize
    member x.indent () = 
      x.IncIndent()
      { new System.IDisposable with 
          member y.Dispose() = 
            x.DecIndent() }
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

  let idVal = new FParsec.IdentifierValidator()

  let quoteIfNeeded (fnName:string) =
    // Todo: check if this is right
    let s, _ = idVal.ValidateAndNormalize(fnName) 
    match s with
    | null -> sprintf "``%s``" s
    | _ -> s

  let renderRouteBuilder (klass:RouteBuilder) (w:FSharpWriter) =
    w.StartWrite <| sprintf "let %s " (quoteIfNeeded <| klass.name)
    using (w.indent()) <| fun _ ->
      let argStrs =
        klass.arguments
        |> List.map (function
          | FunctionParam(Int64Param(name)) -> sprintf "(%s:int64)" (quoteIfNeeded name)
          | FunctionParam(IntParam(name)) -> sprintf "(%s:int)" (quoteIfNeeded name)
          | FunctionParam(StringParam(name)) -> sprintf "(%s:string)" (quoteIfNeeded name))
      let argStr = argStrs |> String.concat " "
      w.Write <| sprintf "%s =\n" argStr

      using (w.indent()) (fun _ ->
        let rec mkRouteBuilder segs (tmpConsts:ResizeArray<_>) (ret:ResizeArray<_>) =
          match segs with
          | [] -> 
            if tmpConsts.Count > 0 then
              let sfx = tmpConsts |> String.concat "/" |> sprintf "\"%s/\""
              ret.Add(sfx)
            ret |> String.concat " + "
          | ConstantSeg(name)::segs ->
            tmpConsts.Add(name)
            mkRouteBuilder segs tmpConsts ret
          | seg::segs ->
            let name = match seg with 
            | Int64Seg(name) | IntSeg(name) | StringSeg(name) -> name 
            | ConstantSeg(_) -> failwith "Logic error"

            if tmpConsts.Count > 0 then
              let pfx = tmpConsts |> String.concat "/" |> sprintf "\"%s/\""
              tmpConsts.Clear()
              ret.Add(pfx)
            let argPart = match seg with
            | Int64Seg(_)
            | IntSeg(_) -> sprintf "%s.ToString()" name
            | StringSeg(_) -> name
            | ConstantSeg(_) -> failwith "Logic error"

            ret.Add(argPart)
            mkRouteBuilder segs tmpConsts ret
        w.StartWriteLine <| mkRouteBuilder (klass.segments) (ResizeArray<_>()) (ResizeArray<_>()))

  let dynParamTypeName (dynParam:DynamicParam) =
    match dynParam with
    | Int64Param(_) -> "int64"
    | IntParam(_) -> "int"
    | StringParam(_) -> "string"

  let paramListTypeString (paramList: DynamicParam list) (options:RouteCompilationArgs) =
    let fnTypes = paramList |> List.map dynParamTypeName |> ResizeArray
    
    if fnTypes.Count = 0 || options.inputType then
      let inTpName = if options.inputType then "'TContext" else "unit"
      fnTypes.Insert(0, inTpName)

    let outTpName = if options.returnType then "'TReturn" else "unit"
    fnTypes.Add(outTpName)

    String.concat "->" fnTypes

  let notFoundHandlerParams = [
    StringParam("verb")
    StringParam("path")
  ]

  let notFoundCtorStr (options:RouteCompilationArgs) =
    let typeStr = paramListTypeString notFoundHandlerParams options
    sprintf "notFound: (%s) option" typeStr

  let renderRecordValues (klassName:string) (ctorParams:HandlerCtorParam list) (options:RouteCompilationArgs) (w:FSharpWriter) =
    w.StartWrite "{"

    let ctorParams = ctorParams |> Array.ofList
    for i in [0..ctorParams.Length - 1] do
      let ctorParam = ctorParams.[i]
      let ctorArgs = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)

      if i = 0 then
        let spaces = new String(' ', w.IndentSize - 1) // take already written opening brace into account
        w.Write <| sprintf "%s%s: %s" spaces (ctorParam.name) (paramListTypeString ctorArgs options) 
      else
        let spaces = new String(' ', w.IndentSize)
        w.StartWrite <| sprintf "%s%s: %s" spaces (ctorParam.name) (paramListTypeString ctorArgs options) 
      
      w.Write("\n")
    w.StartWrite <| sprintf "%s%s" (new String(' ', w.IndentSize)) (notFoundCtorStr options)
    w.Write(" }")

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
        
        for seg, child in constants do
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
          let childIfs = 
            children 
            |> List.map (fun c -> route2If c (idx + 1)) 
            |> List.rev
          { partIdx = Some(idx); seg = seg; children = childIfs }
      | Endpoint(_) as endpoint ->
        // Don't need to worry about children here, because it isn't possible with the way we group routes
        { partIdx = None; seg = endpoint; children = [] }

  type ConditionNode = 
    { preConditionCheck: string option
      assignment: string option
      body: RouteConditionBody }
  and RouteConditionBody =
  | Choices of ConditionNode list
  | Body of string

  let renderCatch (options:RouteCompilationArgs) (w:FSharpWriter) =
    w.StartWriteLine <| "match this.notFound with"
    w.StartWriteLine <| "| None -> raise (Internal.RouteNotMatchedException (verb, path))"
    //if (this.notFound == null) { throw new RouteNotMatchedException(verb, path); }"
    let notFoundArgs' = ["verb"; "path"]
    let notFoundArgs = if options.inputType then "context" :: notFoundArgs' else notFoundArgs'
    let notFoundArgsStr = notFoundArgs |> String.concat " "
    //let retStr = if options.returnType then "return " else ""
    w.StartWriteLine <| sprintf "| Some(notFound) -> notFound %s" notFoundArgsStr

  let if2Cond (routeIf:RouteIfTest) (options:RouteCompilationArgs) : ConditionNode = 
    let rec renderIfAux (routeIf:RouteIfTest) (precedingSegs:NamedRouteSegment list) (depth:int) =
      match routeIf.seg with
      | NamedRouteSegment(seg) ->
        let idx = routeIf.partIdx.Value
        match seg with 
        | ConstantSeg(name) ->
          { preConditionCheck = Some(sprintf "parts.[start + %d] = \"%s\"" idx name)
            assignment = None
            body = routeIf.children |> List.map (fun child -> 
              renderIfAux child (seg :: precedingSegs) (depth + 1)) |> Choices }
        | StringSeg(name) ->
          { preConditionCheck = None
            assignment = Some(sprintf "let %s = parts.[start + %d]" name idx)
            body = routeIf.children |> List.map (fun child ->
              renderIfAux child (seg :: precedingSegs) (depth + 1)) |> Choices }
        | Int64Seg(name)
        | IntSeg(name) ->
          let tname = 
            match seg with
            | IntSeg(_) -> "Int32"
            | Int64Seg(_) -> "Int64"
            | _ -> failwith "Logic error"

          { preConditionCheck = Some(sprintf "Internal.stringIsAllDigits(parts.[start + %d])" idx)
            assignment = Some(sprintf "let %s = %s.Parse(parts.[start + %d])" name tname idx)
            body = routeIf.children |> List.map (fun child ->
              renderIfAux child (seg :: precedingSegs) (depth + 1)) |> Choices }
      | Endpoint(endpoint) ->
        let dynArgs = 
          precedingSegs 
          |> List.rev 
          |> List.choose (function
            | Int64Seg(n) | IntSeg(n) | StringSeg(n) -> Some(n) 
            | ConstantSeg(_) -> None)

        let args = 
          if options.inputType then "context" :: dynArgs
          else dynArgs

        let handlerCall =
          sprintf "this.%s %s" (endpoint.handlerName) (args |> String.concat " ")
        { preConditionCheck = Some(sprintf "verb = \"%s\"" (endpoint.verb))
          assignment = None
          body = Body(handlerCall) }
    renderIfAux routeIf [] 0

  let rec renderRouteConds (routeConds: ConditionNode list) (options: RouteCompilationArgs) (w:FSharpWriter) =
    match routeConds with
    | [] -> failwith "Logic error"
    | [cond] ->
      match cond.preConditionCheck with
      | Some(check) ->
        w.StartWrite <| sprintf "if %s then (* ho *) " check
        w.Write("\n")
        using (w.indent()) <| fun _ ->
          cond.assignment |> Option.iter (fun assig -> w.StartWriteLine assig)  
          renderRouteCondsBody (cond.body) options w
        w.Write("\n")
        w.StartWriteLine("else")    
        using (w.indent()) <| fun _ ->
          renderCatch options w
      | None -> 
        cond.assignment |> Option.iter (fun assig -> w.StartWriteLine assig)  
        renderRouteCondsBody (cond.body) options w    
    | _ ->
      let mutable lastElse = false
      routeConds |> List.iteri (fun i cond ->
        let keyword = 
          if cond.preConditionCheck.IsSome then
            if i = 0 then "if" else "elif"
          else "else"
        let thenS = if keyword = "else" then "" else "then (* hi *)"
        w.StartWrite <| keyword + " "
        match cond.preConditionCheck with
        | Some(check) ->
          w.Write <| sprintf "%s %s\n" check thenS
        | None -> w.Write "\n"
        
        using (w.indent()) <| fun _ ->
          cond.assignment |> Option.iter (fun s -> w.StartWriteLine s)
          renderRouteCondsBody (cond.body) options w
        w.Write("\n")
        
        if keyword = "else" then lastElse <- true)
      if not lastElse then
        w.StartWriteLine("else")
        using (w.indent()) <| fun _ -> renderCatch options w          
  and bodyIndent body =
    match body with 
    | Choices(_) -> true 
    | Body(_) -> false
  and renderRouteCondsBody body (options: RouteCompilationArgs) (w:FSharpWriter) =
    match body with
    | Choices(choices) ->
      renderRouteConds choices options w 
    | Body(s) ->
      let doIndent = bodyIndent body
      if doIndent then w.Write(s) else w.StartWrite(s)

  let renderRouteGroupMatchTest (group:RouteTreeFragment list) (options:RouteCompilationArgs) (w:FSharpWriter) =
    Debug.Print <| sprintf "Group:\n%A" group
    let routeConds = 
      group
      |> List.map (fun route -> route2If route 0)
      |> List.map (fun IF -> if2Cond IF options)

    Debug.Print <| sprintf "RouteConds:\n%A" routeConds
    renderRouteConds routeConds options w

  let renderDispatchMethod (routeTree:RouteNode) (options:RouteCompilationArgs) (w:FSharpWriter) =
    let routeGroups = routeTree |> flattenRouteTree |> groupRoutes

    let baseArgs = ["(verb:string)"; "(path:string)"]

    let args = 
      if options.inputType then "(context:'TContext)" :: baseArgs
      else baseArgs

    let retTp =
      if options.returnType then "'TReturn"
      else "unit"
    
    let argsStr = args |> String.concat " "
    w.StartWriteLine <| sprintf "member this.DispatchRoute %s : %s =" argsStr retTp 
    using (w.indent()) (fun _ ->
      w.StartWriteLine <| "let parts = path.Split('/')"
      // Normalize starting and ending slash
      w.StartWriteLine <| "let start = if parts.[0] = \"\" then 1 else 0"
      w.StartWriteLine <| "let endOffset = if parts.Length > 0 && parts.[parts.Length - 1] = \"\" then 1 else 0"
      w.StartWriteLine <| "match parts.Length - start - endOffset with"
      for n, group in routeGroups do
        w.StartWriteLine <| sprintf "| %d ->" n
        using (w.indent()) (fun _ ->
          renderRouteGroupMatchTest group options w)
      w.StartWriteLine <| "| _ ->"
      using (w.indent()) <| fun _ ->
        renderCatch options w)  
      

  let digitCheckFn = """
  let stringIsAllDigits (s:string) =
    let mutable i = 0
    let mutable foundNonDigit = false
    while i < s.Length && not foundNonDigit do
      let c = s.[i]
      if c < '0' || c > '9' then foundNonDigit <- true
      i <- i + 1
    not foundNonDigit
  """

  let routeNotMatchedEx = """
  exception RouteNotMatchedException of string * string
  """

  let renderMultiLineStr (w:FSharpWriter) (s:string) =
    let mutable startIdx = 0
    for line in s.Split('\n') do
      if not <| System.String.IsNullOrWhiteSpace(line) then
        if startIdx = 0 then startIdx <- line |> Seq.findIndex (fun c -> c <> ' ')
        w.StartWriteLine <| line.Substring(startIdx)

  let renderUtilities (w:FSharpWriter) = 
    w.StartWriteLine "module Internal ="
    using (w.indent()) <| fun _ ->
      renderMultiLineStr w digitCheckFn 
      renderMultiLineStr w routeNotMatchedEx

  let renderMainClass (klass:RouterKlass) (routeTree:RouteNode) (options:RouteCompilationArgs) =
    let w = new FSharpWriter(2)
    w.StartWriteLine <| sprintf "namespace %s" (defaultArg options.nameSpace "IsakSky")
    w.StartWriteLine "open System"
    w.StartWriteLine <| sprintf "module %s =" (defaultArg options.moduleName "RouteProvider")
    using (w.indent()) <| fun _ ->
      for k in klass.routeKlasses do renderRouteBuilder k w
      
      w.Write("\n")
      renderUtilities w

      w.Write("\n")
      let typeArgStr =
        match options.inputType, options.returnType with
        | true, true -> "<'TContext, 'TReturn>"
        | true, false -> "<'TContext>"
        | false, true -> "<'TReturn>"
        | false, false -> ""
      w.StartWriteLine <| sprintf "type %s%s =" (klass.name) typeArgStr
      using (w.indent()) <| fun _ ->
        match klass.ctor with
        | Some(ctorParams) ->
          renderRecordValues (klass.name) ctorParams options w
        | None -> failwith "Missing ctor"
        System.Diagnostics.Debug.Print <| sprintf "RouteTree:\n\n%A" routeTree
        w.Write("\n\n")
        renderDispatchMethod routeTree options w
        
    w.content.ToString()
   
  let compileRoutes (options:RouteCompilationArgs) (output:TextWriter) = 
    let routes = options.parse
    let klass = routes2Class routes options
    let routeTree = buildRouteTree routes
    let code = renderMainClass klass routeTree options
    output.Write(code)