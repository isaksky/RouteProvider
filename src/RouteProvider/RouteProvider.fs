namespace RouteProvider

open System
open System.Reflection
open System.Net
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open FParsec
open TpUtils
open Route

type RouteNode = 
  { routes : RouteWithIdx list; children: RouteNodeChild list} with
  static member empty = { routes = []; children = [] }
and RouteNodeChild = { seg: NamedRouteSegment; node: RouteNode } // this would just be a Tuple, but QuotationHelper chokes on it
and RouteWithIdx = { route: Route2; idx: int}                    // -=-

type RouteMatchResult () =
  do ()
type RouteMatchResult_BadVerb (validVerbs:string[]) =
  inherit RouteMatchResult ()
  member x.validVerbs = validVerbs
type RouteMatchResult_NotFound () =
  inherit RouteMatchResult ()
type RouteMatchResult_Match (routeIdx:int, routeParams:obj[]) =
  inherit RouteMatchResult ()
  member x.routeIdx = routeIdx
  member x.routeParams = routeParams

exception RouteNotFound
exception BadVerb of string[]

[<AutoOpen>]
module Internal = 
  let rec addRoute (routeNode:RouteNode) (segs:NamedRouteSegment list) (idx:int, route:Route2) =
    match segs with
    | [] ->
      { routeNode with routes = {route = route; idx = idx} :: routeNode.routes}
    | seg::tl -> 
      let child = match Seq.tryFind (fun (child) -> child.seg = seg) routeNode.children with
                  | Some (child) -> child.node
                  | None -> RouteNode.empty
      let child = addRoute child tl (idx, route)
      { routeNode with children = {seg = seg; node = child } :: routeNode.children}

  let buildRouteTree (routes: Route2 list) =
      let routesWithIdx = [0..routes.Length - 1] |> List.zip <| routes
      List.foldBack (fun (idx, route) tree -> 
                      addRoute tree route.routeSegments (idx, route)
                     ) routesWithIdx RouteNode.empty 

  let routeTypeName2 (route:Route2) = 
    let segsDesc = route.routeSegments |> Seq.map (function | Constant(s) -> s
                                                            | NumericID(s) -> sprintf "{%s}" s)
                                       |> String.concat "/"
    sprintf "%s %s" route.verb segsDesc

  let rec matchRoute' (verb:string) (parts: string list) (routeParams:ResizeArray<obj>) (routeTree:RouteNode) : RouteMatchResult =
    match parts with
    | part::tl ->
      let constMatch = 
        Seq.tryPick (fun (child) ->
                       match child.seg with | Constant(s) when s = part  -> Some(child.node)
                                            | _ -> None) 
                    routeTree.children

      match constMatch with
      | Some(routeNode) ->
        matchRoute' verb tl routeParams routeNode
      | _ ->
        let ok, num =  Int64.TryParse part
        if ok then
          let numericMatch = Seq.tryPick (fun (child) -> 
                                            match child.seg with | NumericID(_) -> Some(child.node)
                                                                 | _ -> None) 
                                          routeTree.children

          match numericMatch with
          | Some(routeNode) ->
            routeParams.Add(num :> obj)
            matchRoute' verb tl routeParams routeNode
          | None ->
            RouteMatchResult_NotFound() :> RouteMatchResult
        else
          RouteMatchResult_NotFound() :> RouteMatchResult
    | [] ->
      match routeTree.routes |> Seq.tryFind (fun r -> r.route.verb = verb) with
      | Some(route) ->
        RouteMatchResult_Match(route.idx, (routeParams.ToArray())) :> RouteMatchResult
      | _ ->
        let validVerbs = routeTree.routes |> List.map (fun r -> r.route.verb) |> Array.ofList
        RouteMatchResult_BadVerb(validVerbs) :> RouteMatchResult
      
  let matchRoute(verb:string, pathStr:string, routeTree:RouteNode) =
    let partsAry = pathStr.Split('/')

    let parts = if partsAry.Length > 1 && partsAry.[partsAry.Length - 1] = System.String.Empty then 
                  partsAry.[0..partsAry.Length - 2] 
                else partsAry
                |> List.ofArray
    matchRoute' verb parts (ResizeArray<obj>()) routeTree

  let dispatchRoute(verb:string, pathStr:string, routeTree:RouteNode, notFoundHandler:obj, handlers:obj[]) : obj =
    match matchRoute(verb, pathStr, routeTree) with
    | :? RouteMatchResult_Match as m ->
      let handler = handlers.[m.routeIdx] :?> System.Delegate
      handler.DynamicInvoke(m.routeParams)
    | :? RouteMatchResult_NotFound ->
      match notFoundHandler with
      | null ->
        raise <| RouteNotFound
      | :? System.Delegate as handler ->
        let validVerbs : string[] = [||]
        handler.DynamicInvoke(validVerbs)
    | :? RouteMatchResult_BadVerb as badVerbMatch ->
      let validVerbs = badVerbMatch.validVerbs
      match notFoundHandler with
      | null ->
        raise <| BadVerb validVerbs
      | :? System.Delegate as handler ->
        handler.DynamicInvoke(validVerbs)
    | _ as x ->
      failwithf "Match result %A not implemented" x

[<TypeProvider>]
type public RouteProvider(cfg : TypeProviderConfig) as this = 
  inherit TypeProviderForNamespaces()
  let ns = "IsakSky"
  let asm = System.Reflection.Assembly.GetExecutingAssembly()
  let root = ProvidedTypeDefinition(asm, ns, "RouteProvider", Some typeof<obj>)

  let buildTypes (typeName : string) (args : obj []) = 
    match args with 
    | [| :? string as routesStr|] ->
      let routerType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj[]>)
      match runParserOnString RouteParsing.pRoutes () "Routes" routesStr with
      | Success(routes,_, _) ->
        let typeParamPairs = 
          routes 
          |> List.map (fun (route:Route2) ->
            let routesTp = ProvidedTypeDefinition((routeTypeName2 route), Some typeof<obj[]>, HideObjectMethods = true (*, IsErased = false *))
            let dynRouteParams = route.routeSegments 
                                 |> Seq.choose (function | NumericID(s) -> Some (s, typeof<int64>)
                                                         | _ -> None)
                                 |> List.ofSeq

            let ctorParams = dynRouteParams |> List.map (fun (paramName, paramType) -> ProvidedParameter(paramName, paramType))
            routesTp.AddMember 
            <| ProvidedConstructor(ctorParams,
                                    InvokeCode = fun args -> 
                                                  let argsAsObjs = List.map (fun e -> Expr.Coerce(e, typeof<obj>)) args
                                                  Expr.NewArray(typeof<obj>, argsAsObjs)
                                                  )
            let routeProps =
                dynRouteParams |> List.mapi (fun i (paramName, paramType) -> 
                                          ProvidedProperty(paramName, paramType, 
                                                            GetterCode = fun args ->
                                                                          let objExp = <@@ (%%(args.[0]) :> obj[]).[i] @@>
                                                                          Expr.Coerce(objExp, paramType)))
            routesTp.AddMembers routeProps
            // Pull things out of the route record to help out the Quotation evaluater
            let verb = route.verb
            // Make ghetto version of NamedRouteSegment list that is supported with type providers
            let segs = route.routeSegments
                        |> List.map (function | Constant(s) -> 
                                               <@@ ConstantSeg s :> PathSegment @@>
                                              | NumericID(s) -> 
                                               <@@ Int64Seg s :> PathSegment @@>
                                              | _ -> failwith "PathSegment type not implemented")
                                        
            routesTp.AddMember <| ProvidedProperty("verb", typeof<string>, IsStatic = true,
                                                  GetterCode = (fun _ -> <@@ verb @@>))
                          
            routesTp.AddMember <| ProvidedProperty("routeSegments", 
                                                  typeof<PathSegment[]>, IsStatic = true,
                                                  GetterCode = (fun _ -> Expr.NewArray(typeof<PathSegment>, segs)))
            (routesTp, dynRouteParams))
        let routeTree = buildRouteTree routes
        Seq.iter (fst >> routerType.AddMember) typeParamPairs

        let ctorStaticParams = [ProvidedParameter("notFound", typeof<Action<string[]>>, optionalValue = null)]

        let ctorDynParams = 
          typeParamPairs 
          |> List.map (fun (subTp, subTypeParams) ->
                          let subTypeParamTypes = subTypeParams 
                                                  |> (List.map snd) 
                                                  |> Array.ofList
                          let handlerFuncTypeDef = handlerFuncTypeDef <| List.length subTypeParams
                          let handlerFuncType = handlerFuncTypeDef.MakeGenericType(subTypeParamTypes)
                          ProvidedParameter((subTp.Name), handlerFuncType))

        let newTypeCtor = ProvidedConstructor(ctorStaticParams @ ctorDynParams, 
                                              InvokeCode = fun args -> 
                                                let argsAsObjs = List.map (fun e -> Expr.Coerce(e, typeof<obj>)) args
                                                let _, routesExp = QuotationHelpers.recordExpr routeTree
                                                let routesObjExp = Expr.Coerce(routesExp, typeof<obj>)
                                                let selfObjAry = routesObjExp :: argsAsObjs
                                                Expr.NewArray(typeof<obj>, selfObjAry))
        routerType.AddMember newTypeCtor

        routerType.AddMember <| ProvidedMethod("matchRoute", 
                               [ProvidedParameter("verb", typeof<string>); ProvidedParameter("path", typeof<string>)], 
                               typeof<RouteMatchResult>, 
                               InvokeCode = fun args -> 
                                                let selfExp = <@@ (%%(args.[0]) :> obj[]) @@>
                                                let routeTreeExp = <@@ (%%selfExp :> obj[]).[0] :?> RouteNode @@>
                                                let verbExp = <@@ (%%(args.[1]) :> string) @@>
                                                let pathExp = <@@ (%%(args.[2]) :> string) @@>
                                                <@@ Internal.matchRoute(%%verbExp, %%pathExp, %%routeTreeExp) :> RouteMatchResult @@>)

        routerType.AddMember <| ProvidedMethod("dispatchRoute", 
                               [ProvidedParameter("verb", typeof<string>); ProvidedParameter("path", typeof<string>)], 
                               typeof<unit>,
                               InvokeCode = fun args -> 
                                                let selfExp = <@@ (%%(args.[0]) :> obj[]) @@>
                                                let routeTreeExp = <@@ (%%selfExp :> obj[]).[0] :?> RouteNode @@>
                                                let notFoundHandlerExp = <@@ (%%selfExp :> obj[]).[1] :?> obj @@>
                                                let handlersExp = <@@ (%%selfExp :> obj[]).[2..] :> obj[] @@>

                                                let verbExp = <@@ (%%(args.[1]) :> string) @@>
                                                let pathExp = <@@ (%%(args.[2]) :> string) @@>

                                                <@@ Internal.dispatchRoute(%%verbExp, %%pathExp, %%routeTreeExp, %%notFoundHandlerExp, %%handlersExp) @@>

                                                )
      | Failure (msg,_,_) ->
        failwith (sprintf "Failed to parse routes. Error: %s" msg)
      routerType
    | args -> failwith (sprintf "Bad params. Expected 1 string, but got %d params: %A" args.Length args)

  let parameters = [ProvidedStaticParameter("routesStr", typeof<string>)]

  let helpText = """<summary>Typed representation of a set of routes.</summary>
       <param name='RouteString'>The set of routes!</param>"""
  do root.AddXmlDoc helpText
  do root.DefineStaticParameters(parameters, buildTypes)
  do this.AddNamespace(ns, [root])

[<TypeProviderAssembly>]
do ()
