namespace IsakSky.RouteProvider

open System
open System.Reflection
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices

type RouteProvider() = inherit obj()

[<TypeProvider>]
type RouteProviderCore(cfg: TypeProviderConfig) =
  let cfg = cfg
  let namespaceName = "IsakSky"
  let invalidation = new Event<_,_>()
  let mutable _assemblyResolver : ResolveEventHandler = null

  do
    _assemblyResolver <- ResolveEventHandler(fun _ args ->
          let expectedName = (AssemblyName(args.Name)).Name + ".dll"
          let asmPath = 
              cfg.ReferencedAssemblies
              |> Seq.tryFind (fun asmPath -> IO.Path.GetFileName(asmPath) = expectedName)
          match asmPath with
          | Some f -> Assembly.LoadFrom f
          | None -> null)
    System.AppDomain.CurrentDomain.add_AssemblyResolve(_assemblyResolver)

  let staticParams = [|
    { new ParameterInfo() with
        override this.Name with get() = "routes"
        override this.Position with get() = 0
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.None
    }
    { new ParameterInfo() with
        override this.Name with get() = "inputTypeName"
        override this.Position with get() = 1
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.Optional
        override this.RawDefaultValue with get() = "" :> obj
        override this.DefaultValue with get() = "" :> obj
    }
    { new ParameterInfo() with
        override this.Name with get() = "returnTypeName"
        override this.Position with get() = 2
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.Optional
        override this.RawDefaultValue with get() = "" :> obj
        override this.DefaultValue with get() = "" :> obj
    }
  |]  
  //member this.ResolveAssembly(args) =

  interface ITypeProvider with
      [<CLIEvent>]
      member this.Invalidate =
          invalidation.Publish
      member this.GetNamespaces() =
          [| this |]
      member this.GetStaticParameters(typeWithoutArguments) =
        if typeWithoutArguments = typeof<RouteProvider> then
          staticParams
        else 
          [| |]
      member this.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
        let typeName = typeNameWithArguments.[typeNameWithArguments.Length - 1]

        let compilerArgs = 
          match staticArguments with
          | [|:? string as routeStr; :? string as inputTypeName; :? string as returnTypeName|] ->
            { RouteCompiler.RouteProviderOptions.routesStr = routeStr
              RouteCompiler.RouteProviderOptions.typeName = typeName
              RouteCompiler.RouteProviderOptions.inputTypeName = if inputTypeName = "" then None else Some(inputTypeName) 
              RouteCompiler.RouteProviderOptions.returnTypeName = if returnTypeName = "" then None else Some(returnTypeName) 
              RouteCompiler.RouteProviderOptions.config = Some cfg}
          | _ ->
            failwithf "Bad params: %A" staticArguments
        RouteCompiler.compileRoutes compilerArgs
      member this.GetInvokerExpression(syntheticMethodBase, parameters) =
          match syntheticMethodBase with
          | :? ConstructorInfo as ctor ->
              Quotations.Expr.NewObject(ctor, Array.toList parameters) 
          | :? MethodInfo as mi ->
            if mi.IsStatic then
              Quotations.Expr.Call(mi, Array.toList parameters)
            else 
                Quotations.Expr.Call(parameters.[0], mi, Array.toList parameters.[1..])
          | _ ->
              NotImplementedException(sprintf "Not Implemented: ITypeProvider.GetInvokerExpression(%A, %A)" syntheticMethodBase parameters) |> raise
      member this.GetGeneratedAssemblyContents(assembly) =
          IO.File.ReadAllBytes assembly.ManifestModule.FullyQualifiedName
      member this.Dispose() =
        System.AppDomain.CurrentDomain.remove_AssemblyResolve(_assemblyResolver)

  interface IProvidedNamespace with
      member this.ResolveTypeName(typeName) =
          typeof<RouteProvider>
      member this.NamespaceName
          with get() =
              namespaceName
      member this.GetNestedNamespaces() =
          [| |]
      member this.GetTypes() =
          [| typeof<RouteProvider> |]

[<assembly: TypeProviderAssembly>]
do ()

