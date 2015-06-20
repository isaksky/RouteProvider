namespace RouteProvider


// --------------------------------------------------------------------------------------
// The World Bank type provider 
// --------------------------------------------------------------------------------------
namespace ProviderImplementation

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

// Make some Ghetto types that ProviderImplementation.ProvidedTypes.fs's quotation evaluator supports.
// ProviderImplementation.ProvidedTypes.fs does not support records or discriminated unions
type PathSegment (name:string) = 
  member x.name = name
type ConstantSeg (name:string) =
  inherit PathSegment(name)
type Int64Seg (name:string) =
  inherit PathSegment(name)

[<TypeProvider>]
type public RouteProvider(cfg : TypeProviderConfig) as this = 
  inherit TypeProviderForNamespaces()
  let ns = "IsakSky"
  let asm = System.Reflection.Assembly.GetExecutingAssembly()
  let root = ProvidedTypeDefinition(asm, ns, "RouteProvider", Some typeof<obj>)

  let routeTypeName (route:Route.Route) = 
    let segsDesc = route.routeSegments |> Seq.map (function | Constant(s) -> s
                                                            | NumericID(s) -> sprintf "{%s}" s )
                                       |> String.concat "/"
    sprintf "%s %s" route.verb segsDesc
  
  let buildTypes (typeName : string) (args : obj []) = 
    match args with 
    | [| :? string as routesStr; _; _; |] ->
      let newType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
      match runParserOnString RouteParsing.pRoutes () "Routes" routesStr with
      | Success(res,_, _) ->
        res |> Seq.iter (fun (route:Route) ->
                          let routeTp = ProvidedTypeDefinition((routeTypeName route), Some typeof<obj>, HideObjectMethods = true)
                          let ctorParams = route.routeSegments 
                                           |> List.choose (function
                                                          | NumericID(s) -> Some(ProvidedParameter(s, typeof<int64>))
                                                          | _ -> None)

                          routeTp.AddMember <| ProvidedConstructor(ctorParams,InvokeCode = fun args -> <@@ obj() @@>)

                          // Pull things out of the route record to help out the Quotation evaluater in ProvidedTypes.fs
                          let verb = route.verb
                          let segs = route.routeSegments
                                     |> Seq.map (function | NamedRouteSegment.Constant(s) -> <@@ ConstantSeg(s) :> PathSegment @@>
                                                          | NamedRouteSegment.NumericID(s) -> <@@ Int64Seg(s) :> PathSegment @@>)
                                     |> List.ofSeq
                                        
                          routeTp.AddMember <| ProvidedProperty("verb", typeof<string>, IsStatic = true,
                                                                GetterCode = (fun _ -> <@@ verb @@>))
                          
                          routeTp.AddMember <| ProvidedProperty("routeSegments", 
                                                                typeof<PathSegment[]>, IsStatic = true,
                                                                GetterCode = (fun _ -> Expr.NewArray(typeof<PathSegment>, segs)))
                          
                          newType.AddMember(routeTp)
                        )
      | Failure (msg,_,_) ->
        failwith (sprintf "Failed to parse routes. Error: %s" msg)
      newType
    | args -> failwith (sprintf "Bad params. Expected 1 string, but got an array of length %d: %A" args.Length args)

  let parameters = [ProvidedStaticParameter("routesStr", typeof<string>)
                    //ProvidedStaticParameter("ResolutionFolder", typeof<string>, parameterDefaultValue = "")
                    ]

  let helpText = """<summary>Typed representation of a set of routes.</summary>
       <param name='RouteString'>The set of routes!</param>"""
  do root.AddXmlDoc helpText
  do root.DefineStaticParameters(parameters, buildTypes)
  do this.AddNamespace(ns, [root])

[<TypeProviderAssembly>]
do ()
