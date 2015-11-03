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
         //override this.RawDefaultValue with get() = defaultVal
         //override this.DefaultValue with get() = defaultVal
         override this.Attributes with get() = ParameterAttributes.None
      }
    |]
    
    interface ITypeProvider with
        [<CLIEvent>]
        member this.Invalidate =
            //Diagnostics.Debugger.Break()
            printfn "ITypeProvider.Invalidate"
            invalidation.Publish
        member this.GetNamespaces() =
            [| this |]
        member this.GetStaticParameters(typeWithoutArguments) =
            printfn "ITypeProvider.GetStaticParameters(%A)" typeWithoutArguments
            staticParams
        member this.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            printfn "ITypeProvider.ApplyStaticArguments(%A, %A, %A)" typeWithoutArguments typeNameWithArguments staticArguments
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
            //makeVector typeNameWithArguments.[typeNameWithArguments.Length-1] staticArguments
        member this.GetInvokerExpression(syntheticMethodBase, parameters) =
            printfn "ITypeProvider.GetInvokerExpression(%A, %A)" syntheticMethodBase parameters
            // this method only needs to be implemented for a generated type provider if
            // you are using the declared types from in the same project or in a script
            //NotImplementedException(sprintf "Not Implemented: ITypeProvider.GetInvokerExpression(%A, %A)" syntheticMethodBase parameters) |> raise
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
            printfn "IProvidedNamespace.ResolveTypeName(%A)" typeName
            typeof<Routes>
        member this.NamespaceName
            with get() =
                printfn "IProvidedNamespace.NamespaceName.get()";
                namespaceName
        member this.GetNestedNamespaces() =
            printfn "IProvidedNamespace.GetNestedNamespaces()"
            [| |]
        member this.GetTypes() =
            printfn "IProvidedNamespace.GetTypes()"
            [| typeof<Routes> |]

[<assembly: TypeProviderAssembly>]
do ()