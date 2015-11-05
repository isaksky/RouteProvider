namespace RouteProvider

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices

type Routes() = inherit obj()

[<TypeProvider>]
type RouteProvider() =
    let namespaceName = "IsakSky"
    let invalidation = new Event<_,_>()

    let staticParams = [|
      { new ParameterInfo() with
         override this.Name with get() = "routes"
         override this.Position with get() = 0
         override this.ParameterType with get() = typeof<string>
         override this.Attributes with get() = ParameterAttributes.None
      }
    |]
    
    interface ITypeProvider with
        [<CLIEvent>]
        member this.Invalidate =
            invalidation.Publish
        member this.GetNamespaces() =
            [| this |]
        member this.GetStaticParameters(typeWithoutArguments) =
            staticParams
        member this.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            let typeName = typeNameWithArguments.[typeNameWithArguments.Length - 1]

            match staticArguments with
            | [|:? string as routeStr|] ->
              let compilerArgs = { 
                RouteCompiler.RouteProviderOptions.routesStr = routeStr
                RouteCompiler.RouteProviderOptions.typeName = typeName 
              }
              RouteCompiler.compileRoutes compilerArgs
            | _ ->
              failwithf "Bad params: %A" staticArguments
        member this.GetInvokerExpression(syntheticMethodBase, parameters) =
            match syntheticMethodBase with
            | :? ConstructorInfo as ctor ->
                Quotations.Expr.NewObject(ctor, Array.toList parameters) 
            | :? MethodInfo as mi ->
                Quotations.Expr.Call(parameters.[0], mi, Array.toList parameters.[1..])
            | _ ->
                NotImplementedException(sprintf "Not Implemented: ITypeProvider.GetInvokerExpression(%A, %A)" syntheticMethodBase parameters) |> raise
        member this.GetGeneratedAssemblyContents(assembly) =
            IO.File.ReadAllBytes assembly.ManifestModule.FullyQualifiedName
        member this.Dispose() =
            ()

    interface IProvidedNamespace with
        member this.ResolveTypeName(typeName) =
            typeof<Routes>
        member this.NamespaceName
            with get() =
                namespaceName
        member this.GetNestedNamespaces() =
            [| |]
        member this.GetTypes() =
            [| typeof<Routes> |]

[<assembly: TypeProviderAssembly>]
do ()