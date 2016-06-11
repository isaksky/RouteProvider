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
open System.Reflection

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
    | GuidParam of string
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
        | StringSeg(name) -> Some(FunctionParam(StringParam(name)))
        | GuidSeg(name) -> Some(FunctionParam(GuidParam(name))))
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
                        | GuidSeg(name) -> Some(FunctionParam(GuidParam(name)))
                        | ConstantSeg(_) -> None)
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
    let mutable lastNewLine = false
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
      if s.Length > 0 then
        lastNewLine <- s.[s.Length - 1] = '\n'
      x.content.Append(s) |> ignore
    member x.StartWrite(str:string) =
      let pfx = new string(' ', indentation)
      let s = sprintf "%s%s" pfx str
      if str.Length > 0 then
        lastNewLine <- str.[str.Length - 1] = '\n'
      x.content.Append(s) |> ignore
    member x.Write(str:string) =
      if str.Length > 0 then
        lastNewLine <- str.[str.Length - 1] = '\n'
      x.content.Append(str) |> ignore
    member x.Newline() =
      if not lastNewLine then
        x.Write("\n")
        lastNewLine <- true

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
          | FunctionParam(StringParam(name)) -> sprintf "(%s:string)" (quoteIfNeeded name)
          | FunctionParam(GuidParam(name)) -> sprintf "(%s:Guid)" (quoteIfNeeded name))
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
                       | Int64Seg(name) | IntSeg(name) | StringSeg(name) | GuidSeg(name) -> name
                       | ConstantSeg(_) -> failwith "Logic error"

            if tmpConsts.Count > 0 then
              let pfx = tmpConsts |> String.concat "/" |> sprintf "\"%s/\""
              tmpConsts.Clear()
              ret.Add(pfx)
            let argPart =
              match seg with
              | Int64Seg(_)
              | IntSeg(_) -> sprintf "%s.ToString()" name
              | GuidSeg(_) -> sprintf "%s.ToString(\"D\")" name
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
    | GuidParam(_) -> "Guid"

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

  let notFoundCtorStr (options:RouteCompilationArgs) inRecord =
    let typeStr = paramListTypeString notFoundHandlerParams options
    let delim = if inRecord then ":" else " ="
    sprintf "notFound%s (%s) option" delim typeStr

  let renderRecordValues (ctorParams:HandlerCtorParam list) (options:RouteCompilationArgs) (w:FSharpWriter) =
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
    w.StartWrite <| sprintf "%s%s" (new String(' ', w.IndentSize)) (notFoundCtorStr options true)
    w.Write(" }")

  let renderCreateFunction (klassName:string) (ctorParams:HandlerCtorParam list) (options:RouteCompilationArgs) (w:FSharpWriter) =
    let retTypeStr =
      if options.inputType && options.returnType then klassName + "<_,_>"
      elif options.inputType || options.returnType then klassName + "<_>"
      else klassName

    let notFoundFnStr = paramListTypeString notFoundHandlerParams options
    let fnStartStr = "static member Router("
    w.StartWrite fnStartStr

    let ctorParams = ctorParams |> Array.ofList
    let fnArgSpaces = new String(' ', fnStartStr.Length)
    for i in [0..ctorParams.Length - 1] do
      let ctorParam = ctorParams.[i]
      let ctorArgs = ctorParam.handlerArgs |> List.map (function | FunctionParam(p) -> p)
      let sp = if i = 0 then "" else fnArgSpaces
      let wr = if i = 0 then w.Write else w.StartWrite
      wr <| sprintf "%s%s: %s" sp (ctorParam.name) (paramListTypeString ctorArgs options)
      w.Write(",\n")

    w.StartWrite <| sprintf "%s?notFound: %s" fnArgSpaces notFoundFnStr
    w.Write <| sprintf ") : %s =\n" retTypeStr
    using (w.indent()) <| fun _ ->
      w.StartWrite "{"
      for i in [0..ctorParams.Length - 1] do
        let ctorParam = ctorParams.[i]

        if i = 0 then
          let spaces = new String(' ', w.IndentSize - 1) // take already written opening brace into account
          w.Write <| sprintf "%s%s = %s\n" spaces (ctorParam.name) (ctorParam.name)
        else
          let spaces = new String(' ', w.IndentSize)
          w.StartWrite <| sprintf "%s%s = %s\n" spaces (ctorParam.name) (ctorParam.name)
      w.StartWrite <| sprintf "%snotFound = notFound" (new String(' ', w.IndentSize))
      w.Write "}"

  type RouteNode =
    { endPoints: Endpoint list
      children: (NamedRouteSegment * RouteNode) list
      depth: int }
    static member Empty () =
      { endPoints = []
        children = []
        depth = 0 }
  and Endpoint =
    { verb: string; handlerName: string; segments: NamedRouteSegment list}

  let routeEndPoint (route:Route) =
    { verb = route.verb; handlerName = (handlerName route); segments = route.routeSegments}

  let rec buildNode (endPoint:Endpoint) (segsRemaining: NamedRouteSegment list) (depth: int) =
    match segsRemaining with
    | [] ->
      { endPoints = [endPoint]; children = []; depth = depth; }
    | seg::segs ->
      let child = seg, (buildNode endPoint segs (depth + 1))
      { endPoints = []; children = [child]; depth = depth; }

  let rec addRoute (routeNode:RouteNode) (endPoint:Endpoint) (segsRemaining:NamedRouteSegment list) (depth:int) =
    match segsRemaining with
    | [] ->
      { routeNode with endPoints = routeNode.endPoints @ [endPoint] }
    | seg::segs ->
      let existingChild =
        routeNode.children
        |> Seq.tryFind (fun (s, _) -> s = seg)
      match existingChild with
      | None ->
        let newChild = seg, buildNode endPoint segs (depth + 1)
        { routeNode with children = routeNode.children @ [newChild]}
      | Some(existingChild) ->
        let updatedChildren =
          routeNode.children
          |> List.map (fun child ->
            if child = existingChild then
              let seg, routeNode = child
              (seg, (addRoute routeNode endPoint segs (depth + 1)))
            else
              child)
        { routeNode with children = updatedChildren }

  let buildRouteTree (routes:Route list) =
    let rec addRouteF routeNode routes =
      match routes with
      | route :: routes ->
        let updatedRouteNode = addRoute routeNode (routeEndPoint route) (route.routeSegments) 0
        addRouteF updatedRouteNode routes
      | [] ->
        routeNode

    addRouteF (RouteNode.Empty()) routes

  let renderNotFoundCall (options:RouteCompilationArgs) (w:FSharpWriter) =
    let notFoundArgs' = ["verb"; "path"]
    let notFoundArgs = if options.inputType then "context" :: notFoundArgs' else notFoundArgs'
    let notFoundArgsStr = notFoundArgs |> String.concat ", "
    w.Write <| sprintf "this.HandleNotFound(%s)\n" notFoundArgsStr

  let renderNotFoundHandler (options:RouteCompilationArgs) (w:FSharpWriter) =
    let notFoundArgs' = ["verb"; "path"]
    let notFoundArgs = if options.inputType then "context" :: notFoundArgs' else notFoundArgs'
    let argStr = notFoundArgs |> String.concat ", "
    w.StartWriteLine <| sprintf "member inline private this.HandleNotFound(%s) =" argStr

    using (w.indent()) <| fun _ ->
      w.StartWriteLine <| "match this.notFound with"
      w.StartWriteLine <| "| None -> raise (Internal.RouteNotMatchedException (verb, path))"
      let notFoundArgsStr = notFoundArgs |> String.concat " "
      w.StartWriteLine <| sprintf "| Some(notFound) -> notFound %s" notFoundArgsStr

  let getBranches (routeTree:RouteNode) : (NamedRouteSegment list * Endpoint) seq =
    let rec getBranches' (routeTree:RouteNode) (preSegs : NamedRouteSegment list) =
      seq {
        for endP in routeTree.endPoints do
          yield (List.rev preSegs), endP

        for seg, subTree in routeTree.children do
          yield! (getBranches' subTree (seg::preSegs))
      }
    getBranches' routeTree []

  let getDynamicParams (segs:NamedRouteSegment list) =
    segs
    |> List.choose(
      function
      | IntSeg(name) -> Some <| IntParam(name)
      | Int64Seg(name) -> Some <| Int64Param(name)
      | StringSeg(name) -> Some <| StringParam(name)
      | GuidSeg(name) -> Some <| GuidParam(name)
      | ConstantSeg(_) -> None)

  let captureEq (seg1:NamedRouteSegment) (seg2:NamedRouteSegment) =
    match seg1, seg2 with
    | ConstantSeg(a), ConstantSeg(b) when a = b -> true
    | Int64Seg(_), Int64Seg(_) -> true
    | IntSeg(_), IntSeg(_) -> true
    | StringSeg(_), StringSeg(_) -> true
    | GuidSeg(_), GuidSeg(_) -> true
    | _ -> false

  let segAssigns (seg:NamedRouteSegment) =
    match seg with
    | Int64Seg(_)
    | IntSeg(_)
    | GuidSeg(_) -> true
    | _ -> false

  let segName (seg:NamedRouteSegment) =
    match seg with
    | Int64Seg(name)
    | IntSeg(name)
    | StringSeg(name)
    | GuidSeg(name)
    | ConstantSeg(name) -> name

  let assignNameClash (seg1:NamedRouteSegment) (seg2:NamedRouteSegment) =
    match segAssigns seg1, segAssigns seg2 with
    | true, true when seg1 <> seg2 && segName(seg1) = segName(seg2) -> true
    | _ -> false

  let genericSeg (seg:NamedRouteSegment) (depth:int) =
    match seg with
    | Int64Seg(_) -> Int64Seg(sprintf "int64ArgDepth_%d" depth)
    | IntSeg(_) -> IntSeg(sprintf "intArgDepth_%d" depth)
    | StringSeg(_) -> StringSeg(sprintf "stringArgDepth_%d" depth)
    | GuidSeg(_) -> GuidSeg(sprintf "guidArgDepth_%d" depth)
    | _ -> failwith "Logic error"

  let resolveNameClashes (seg:NamedRouteSegment) (routeTree:RouteNode) =
    match routeTree.children |> List.tryFind (fun (seg2, _) -> assignNameClash seg seg2) with
    | Some(seg2, _) ->
      let newSeg2 = genericSeg seg2 (routeTree.depth)
      let newSeg = genericSeg seg (routeTree.depth)
      let newChildren =
        routeTree.children
        |> List.map(fun (tmpSeg, c) ->
          if Object.ReferenceEquals(tmpSeg, seg2) then
            newSeg2, c
          else
            tmpSeg, c)
      newSeg, { routeTree with children = newChildren }
    | None -> seg, routeTree

  let groupNodesByLength (routeTree:RouteNode) : (int * RouteNode) list =
    let rec addG (routeTree:RouteNode) (segs:NamedRouteSegment list, endpoint:Endpoint) =
      match segs with
      | [] ->
        { routeTree with endPoints = endpoint :: routeTree.endPoints }
      | seg::segs ->
        let seg, routeTree = resolveNameClashes seg routeTree
        match routeTree.children |> List.tryFindIndex(fun (seg2, _) -> captureEq seg seg2) with
        | Some(idx) ->
          let seg2, existingChild = routeTree.children |> List.item idx

          // Might need to make variable name generic, if we have multiple routes that capture the
          // same type and position with diff variable names
          let newSeg =
            if seg = seg2 then seg
            else genericSeg seg (routeTree.depth)

          let updatedChild = addG existingChild (segs, endpoint)
          let updatedChildren =
            routeTree.children
            |> List.mapi(fun i child ->
              if i = idx then (newSeg, updatedChild) else child)
          { routeTree with children = updatedChildren }
        | None ->
          { routeTree with children = (seg, buildRouteNode(routeTree.depth + 1, segs, endpoint)) :: routeTree.children }

    and buildRouteNode (startDepth: int, segs:NamedRouteSegment list, endpoint:Endpoint) =
      segs
      |> List.rev
      |> List.fold
        (fun node seg ->
          //Utility.log "node: %A depth: %d" node (node.depth - 1)
          { children = [seg, node]; endPoints = []; depth = node.depth - 1})
        { children = []; endPoints = [endpoint]; depth = startDepth + segs.Length}

    let addTree (treeGroups: (int * RouteNode) list) (len:int, routeParts) : (int * RouteNode) list =
      match treeGroups |> List.tryFindIndex (fun (n, _) -> n = len) with
      | Some(idx) ->
        let _, node = Seq.item idx treeGroups
        let updatedNode = addG node routeParts
        treeGroups
        |> List.mapi (fun i (n, node) ->
          if i = idx then
            len, updatedNode
          else
            n, node)
      | None ->
        let segs, endP = routeParts
        let newNode = buildRouteNode (0, segs, endP)
        (len, newNode) :: treeGroups

    routeTree
    |> getBranches
    |> Seq.fold
      (fun acc routeParts ->
        let segs, _ = routeParts
        let n = List.length segs
        addTree acc (n, routeParts))
      []

  let inline dbgComment (w:FSharpWriter) lbl a  =
    let pprint = sprintf "%s\n%A" lbl a
    for line in pprint.Split([|"\r\n"; "\n" |], StringSplitOptions.None) do
      w.StartWriteLine <| sprintf "// %s" line

  let segScore = function
    | ConstantSeg(_) -> 10
    | Int64Seg(_) -> 20
    | IntSeg(_) -> 30
    | GuidSeg(_) -> 35
    | StringSeg(_) -> 40

  let noOptDisp =
    { new System.IDisposable with
          member y.Dispose() = () }

  let renderSegmentTests (seg:NamedRouteSegment) (scope:(int * DynamicParam) list) (isFirst:bool) (depth:int) (_:RouteCompilationArgs) (w:FSharpWriter) =
    let kwd = if isFirst then "if" else "elif"
    match seg with
    | ConstantSeg(name) ->
      w.StartWriteLine <| sprintf "%s String.Equals(parts.[%d + start],\"%s\") then" kwd depth name
      scope, w.indent()
    | Int64Seg(name) ->
      w.StartWriteLine <| sprintf "%s Int64.TryParse(parts.[%d + start], &%s) then" kwd depth name
      (depth, Int64Param(name)) :: scope, w.indent()
    | IntSeg(name) ->
      w.StartWriteLine <| sprintf "%s Int32.TryParse(parts.[%d + start], &%s) then" kwd depth name
      (depth, IntParam(name)) :: scope, w.indent()
    | GuidSeg(name) ->
      w.StartWriteLine <| sprintf "%s Guid.TryParseExact(parts.[%d + start], \"D\", &%s) then" kwd depth name
      (depth, IntParam(name)) :: scope, w.indent()
    | StringSeg(name) ->
      if isFirst then
        (depth, StringParam(name))::scope, noOptDisp
      else
        w.StartWriteLine "else"
        (depth, StringParam(name))::scope, w.indent()

  let rec renderRouteNodeCondTree (routeTree:RouteNode) (scope:(int * DynamicParam) list) (options:RouteCompilationArgs) (w:FSharpWriter) =
    match routeTree.endPoints, routeTree.children with
    | [], [] -> failwith "Logic error"
    | endPoints, [] ->
      let numEndPoints = List.length endPoints
      endPoints |> List.iteri(fun i endP ->
        let endpointScope = endP.segments |> getDynamicParams

        // We copy parts of the RouteNode tree in cases where a segment being parsable
        // as an int would otherwise prevent it from being used as a string, so rewrite
        // the scope to handle that here
        let scope =
          (List.rev scope, endpointScope) ||> List.map2 (fun condSeg routeSeg ->
            match condSeg, routeSeg with
            | (n, _), StringParam(_) -> n, routeSeg
            | condSeg, _ -> condSeg)

        let argStr =
          scope
          |> List.map (function
            | _, Int64Param(name)
            | _, IntParam(name)
            | _, GuidParam(name)-> name
            | n, StringParam(_) -> sprintf "(parts.[%d + start])" n)
          |> fun ss -> if options.inputType then "context" :: ss else ss
          |> String.concat " "

        let keyword = if i = 0 then "if" else "elif"
        let invocation =
          if argStr.Length > 0 then sprintf "this.%s %s" (endP.handlerName) argStr
          else sprintf "this.%s()" (endP.handlerName)
        w.StartWriteLine <| sprintf "%s verb = \"%s\" then %s" keyword (endP.verb) invocation
        let isLast = i = numEndPoints - 1
        if isLast then
          w.StartWrite <| "else "
          renderNotFoundCall options w
          w.Newline())
    | [], children ->
      let children = children |> List.sortBy (fst >> segScore)

      // declare vars for TryParse methods to use
      for cseg, _ in children do
        match cseg with
        | Int64Seg(name) ->
          w.StartWriteLine <| sprintf "let mutable %s = 0L" name
        | IntSeg(name) ->
          w.StartWriteLine <| sprintf "let mutable %s = 0" name
        | GuidSeg(name) ->
          w.StartWriteLine <| sprintf "let mutable %s = Guid.Empty" name
        | StringSeg(_)
        | ConstantSeg(_) -> ()

      let numChildren = List.length children
      children
      |> List.iteri (fun i (seg, child) ->
        let isFirst = i = 0

        let newScope, writerScope = renderSegmentTests seg scope isFirst (routeTree.depth) options w
        using writerScope <| fun _ ->
          renderRouteNodeCondTree child newScope options w

        let isLast = i = numChildren - 1
        let isStringSeg = match seg with | StringSeg(_) -> true | _ -> false
        if isLast && not (isStringSeg) then
          w.StartWrite "else "
          renderNotFoundCall options w)

    | _ -> failwith "Logic error"
    //w.StartWriteLine <| sprintf "%A" routeTree

  // Handle how a path segment being a valid int can prevent it from being used a string
  let rec resolveBranchOverlap (routeTree:RouteNode) =
    match routeTree.children with
    | [] -> routeTree
    | children ->
      match children |> List.tryFind(fun (seg, _) -> match seg with | StringSeg(_) -> true | _ -> false) with
      | Some(_, strChild) ->
        let newChildren =
          children
          |> List.map (fun (seg, child) ->
            match seg with
            | Int64Seg(_)
            | IntSeg(_) ->
              seg, { child with
                      endPoints = child.endPoints @ strChild.endPoints
                      children = child.children @ strChild.children }
            | GuidSeg(_)
            | StringSeg(_)
            | ConstantSeg(_) ->
              seg, child)
        { routeTree with children = newChildren |> List.map (fun (seg, child) -> seg, resolveBranchOverlap child)}
      | None ->
        { routeTree with children = routeTree.children |> List.map (fun (seg, child) -> seg, resolveBranchOverlap child)}

  let renderDispatchMethods (routeTree:RouteNode) (options:RouteCompilationArgs) (w:FSharpWriter) =
    let nodesWithLength =
      routeTree
      |> groupNodesByLength
      |> List.map (fun (i, routeNode) -> i, resolveBranchOverlap routeNode)

    let baseArgs = ["verb:string"; "path:string"]

    let args =
      if options.inputType then "context:'TContext" :: baseArgs
      else baseArgs

    let retTp =
      if options.returnType then "'TReturn"
      else "unit"

    let argsStr = args |> String.concat ", "
    w.StartWriteLine <| sprintf "member this.DispatchRoute(%s) : %s =" argsStr retTp
    using (w.indent()) (fun _ ->
      w.StartWriteLine <| "let parts = path.Split('/')"
      // Normalize starting and ending slash
      w.StartWriteLine <| "let start = if parts.[0] = \"\" then 1 else 0"
      w.StartWriteLine <| "let endOffset = if parts.Length > 0 && parts.[parts.Length - 1] = \"\" then 1 else 0"
      w.StartWriteLine <| "match parts.Length - start - endOffset with"
      for n, node in nodesWithLength do
        w.StartWriteLine <| sprintf "| %d ->" n
        using (w.indent()) (fun _ ->
          renderRouteNodeCondTree node [] options w)
      w.StartWriteLine <| "| _ ->"
      using (w.indent()) <| fun _ ->
        w.StartWrite("") // for the not found call below
        renderNotFoundCall options w)

    let args2 = Array.ofList args
    args2.[args2.Length - 1] <- "uri:Uri"
    let argsStr2 = args2 |> String.concat ", "
    let invArgs = ["verb"; "path"]
    let invArgs = if options.inputType then "context" :: invArgs else invArgs
    let invArgsStr = invArgs |> String.concat ", "
    w.Write("\n")
    w.StartWriteLine <| sprintf "member this.DispatchRoute(%s) : %s =" argsStr2 retTp
    using (w.indent()) <| fun _ ->
      w.StartWriteLine "// Ensure we have an Absolute Uri, or just about every method on Uri chokes"
      w.StartWriteLine "let uri = if uri.IsAbsoluteUri then uri else new Uri(Internal.fakeBaseUri, uri)"
      w.StartWriteLine "let path = uri.GetComponents(UriComponents.Path, UriFormat.Unescaped)"
      w.StartWriteLine <| sprintf "this.DispatchRoute(%s)" invArgsStr

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
      w.StartWriteLine("let fakeBaseUri = new Uri(\"http://a.a\")")
      w.Write("\n")
      renderMultiLineStr w routeNotMatchedEx

  let renderMainClass (klass:RouterKlass) (routeTree:RouteNode) (options:RouteCompilationArgs) =
    let w = new FSharpWriter(2)
    let version = Assembly.GetExecutingAssembly().GetName().Version.ToString()
    w.StartWriteLine <| sprintf "// Generated by RouteProvider %s" version
    w.StartWriteLine <| sprintf "namespace %s" (defaultArg options.nameSpace "MyNamespace")
    w.Write("\n")
    w.StartWriteLine "open System"
    w.StartWriteLine <| sprintf "module %s =" (defaultArg options.moduleName "MyModule")
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
          renderRecordValues ctorParams options w
        | None -> failwith "Missing ctor"
        System.Diagnostics.Debug.Print <| sprintf "RouteTree:\n\n%A" routeTree
        w.Write("\n\n")
        renderNotFoundHandler options w
        w.Write("\n")
        renderDispatchMethods routeTree options w
        w.Write("\n")
        renderCreateFunction (klass.name) (klass.ctor.Value) options w

    w.content.ToString()

  let compileRoutes (options:RouteCompilationArgs) (output:TextWriter) =
    let routes = options.parse
    let klass = routes2Class routes options
    let routeTree = buildRouteTree routes
    let code = renderMainClass klass routeTree options
    output.Write(code)
