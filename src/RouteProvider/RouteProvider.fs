namespace IsakSky.RouteProvider

open System
open System.Reflection
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Collections.Concurrent
open System.Runtime.Serialization
open System.Diagnostics.Tracing
open System.Diagnostics
open System.Text
open System.Reflection

open Utility

open RouteCompilation

type RouteProvider() = inherit obj()

[<TypeProvider>]
type RouteProviderCore(cfg: TypeProviderConfig) =
  let cfg = cfg
  let namespaceName = "IsakSky"
  let invalidation = new Event<EventHandler,EventArgs>()
  let mutable _assemblyResolver : ResolveEventHandler = null
  static let _asmBytesCache = BoundedCache<string, byte[]>(10)
  static let _parserResultsCache = BoundedCache<_,_>(30)
  static let _emmiters = ConcurrentDictionary<string, RouterEmitter>(2, 10)

  do
    _assemblyResolver <- ResolveEventHandler(fun _ args ->
          let expectedName = (AssemblyName(args.Name)).Name + ".dll"
          let asmPath =
              cfg.ReferencedAssemblies
              |> Seq.tryFind (fun asmPath -> IO.Path.GetFileName(asmPath) = expectedName)
          match asmPath with
          | Some f -> Assembly.LoadFrom f
          | None -> null)
    AppDomain.CurrentDomain.add_AssemblyResolve(_assemblyResolver)

  let staticParams = [|
    { new ParameterInfo() with
        override this.Name with get() = "typeName"
        override this.Position with get() = 0
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.None
    }
    { new ParameterInfo() with
        override this.Name with get() = "routes"
        override this.Position with get() = 1
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.None
    }
    { new ParameterInfo() with
        override this.Name with get() = "inputType"
        override this.Position with get() = 2
        override this.ParameterType with get() = typeof<bool>
        override this.Attributes with get() = ParameterAttributes.Optional
        override this.RawDefaultValue with get() = false :> obj
        override this.DefaultValue with get() = false :> obj
    }
    { new ParameterInfo() with
        override this.Name with get() = "returnType"
        override this.Position with get() = 3
        override this.ParameterType with get() = typeof<bool>
        override this.Attributes with get() = ParameterAttributes.Optional
        override this.RawDefaultValue with get() = false :> obj
        override this.DefaultValue with get() = false :> obj
    }
    { new ParameterInfo() with
        override this.Name with get() = "outputPath"
        override this.Position with get() = 4
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.Optional
        override this.RawDefaultValue with get() = "" :> obj
        override this.DefaultValue with get() = "" :> obj
    }
    { new ParameterInfo() with
        override this.Name with get() = "nameSpace"
        override this.Position with get() = 5
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.Optional
        override this.RawDefaultValue with get() = "" :> obj
        override this.DefaultValue with get() = "" :> obj
    }
    { new ParameterInfo() with
        override this.Name with get() = "moduleName"
        override this.Position with get() = 6
        override this.ParameterType with get() = typeof<string>
        override this.Attributes with get() = ParameterAttributes.Optional
        override this.RawDefaultValue with get() = "" :> obj
        override this.DefaultValue with get() = "" :> obj
    }
  |]

  static member GetOrCreateFakeAsm(typeName, tpTmpDir) =
    match _asmBytesCache.TryGet(typeName) with
    | Some(bytes) ->
      let asm = Assembly.Load(bytes)
      asm.GetType <| "IsakSky." + typeName
    | None ->
      let code = sprintf "namespace IsakSky { public class %s { } }" typeName
      let dllFile = getTmpFileName tpTmpDir "dll"
      let compilerArgs =  dict [("CompilerVersion", "v4.0")]
      let compiler = new Microsoft.CSharp.CSharpCodeProvider(compilerArgs)
      let parameters = new System.CodeDom.Compiler.CompilerParameters([|"System.dll"|])
      // TODO - see if we can get compilation in memory to work
      parameters.OutputAssembly <- dllFile
      parameters.CompilerOptions <- "/t:library"
      let compilerResults = compiler.CompileAssemblyFromSource(parameters, [| code |])
      if compilerResults.Errors.HasErrors then
        let errors = seq { for err in compilerResults.Errors do yield err }
        failwithf "Got error: %A" (errors |> Seq.head)
      else
        let bytes = File.ReadAllBytes(dllFile)
        _asmBytesCache.Add(typeName, bytes)
        let asm = Assembly.Load(bytes)
        asm.GetType <| "IsakSky." + typeName

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
      member this.ApplyStaticArguments(_ (*typeWithoutArguments *), typeNameWithArguments, staticArguments) =
        let dummyTypeName = typeNameWithArguments.[typeNameWithArguments.Length - 1]

        let typeName, routesStr, inputType, returnType, outputPath, nameSpace, moduleName  =
          match staticArguments with
          | [|:? string as typeName
              :? string as routesStr
              :? bool as inputType
              :? bool as returnType
              :? string as outputPath
              :? string as nameSpace
              :? string as moduleName|] ->
              typeName, routesStr, inputType, returnType, outputPath, nameSpace, moduleName
          | _ ->
            failwithf "Bad params: %A" staticArguments

        let parseResult =
          match _parserResultsCache.TryGet routesStr with
          | Some(p) -> p
          | None ->
            let parse = RouteParsing.parseRoutes routesStr
            _parserResultsCache.Add((routesStr), parse)
            parse

        let resolvedOutputPath = resolvePath outputPath (cfg.ResolutionFolder)

        match parseResult with
        | RouteParsing.RouteParseResult.Failure(msg) ->
          failwithf "%s" msg
        | RouteParsing.RouteParseResult.Success(routes) ->
          let em =
            _emmiters.GetOrAdd(resolvedOutputPath, fun resolvedOutputPath ->
              let threadName = Threading.Thread.CurrentThread.ManagedThreadId
              //log "[RouteProvider]: Thread %d, Instance %A creating a RouterEmitter for %s" threadName (ObjectUtilities.GetInstanceId(this)) resolvedOutputPath
              let em = RouterEmitter(resolvedOutputPath)
              let listenerRef : IDisposable ref = ref null
              let listener = em.Expired.Subscribe(fun _ ->
                _emmiters.TryRemove(resolvedOutputPath) |> ignore
                //log "[RouteProvider]: Thread %d, Instance %A shutting down RouterEmitter for %s" threadName (ObjectUtilities.GetInstanceId(this)) resolvedOutputPath
                (!listenerRef).Dispose())
              listenerRef := listener
              em
            )

          let routeEmitArgs =
            { typeName = typeName
              parse = routes
              outputPath = outputPath
              inputType = inputType
              returnType = returnType
              nameSpace = if nameSpace = "" then None else Some(nameSpace)
              moduleName = if moduleName = "" then None else Some(moduleName) }

          if Assembly.GetCallingAssembly().GetName().Name <> "FSharp.LanguageService.Compiler" then
            match em.PostMessage(routeEmitArgs) with
            | IgnoredStale ->
              RouteProviderCore.GetOrCreateFakeAsm(dummyTypeName, cfg.TemporaryFolder)
            | IgnoredBadExtension ->
              failwith "Bad output file extension. Only *.fs supported"
            | RefuseFilenameTaken ->
              failwithf "Refusing to overwrite file \"%s\". Filename is taken, and does not look generated." resolvedOutputPath
            | Ok | OkSecondaryThread ->
              //log "Triggering invalidation..."
              invalidation.Trigger(this, new EventArgs())
              RouteProviderCore.GetOrCreateFakeAsm(dummyTypeName, cfg.TemporaryFolder)
            else
              RouteProviderCore.GetOrCreateFakeAsm(dummyTypeName, cfg.TemporaryFolder)
      member this.GetInvokerExpression(syntheticMethodBase, parameters) =
          match syntheticMethodBase with
          | :? ConstructorInfo as ctor ->
              Quotations.Expr.NewObject(ctor, Array.toList parameters)
          | :? MethodInfo as mi ->
            if mi.IsStatic then Quotations.Expr.Call(mi, Array.toList parameters)
            else Quotations.Expr.Call(parameters.[0], mi, Array.toList parameters.[1..])
          | _ ->
            let exStr = sprintf "Not Implemented: ITypeProvider.GetInvokerExpression(%A, %A)" syntheticMethodBase parameters
            raise <| NotImplementedException(exStr)
      member this.GetGeneratedAssemblyContents(assembly) =
        assembly.GetTypes()
        |> Array.tryPick (fun typ -> _asmBytesCache.TryGet (typ.Name))
        |> Option.get
      member this.Dispose() =
        //log "this.Dispose()!"
        AppDomain.CurrentDomain.remove_AssemblyResolve(_assemblyResolver)

  interface IProvidedNamespace with
      member this.ResolveTypeName(_) = typeof<RouteProvider>
      member this.NamespaceName
          with get() = namespaceName
      member this.GetNestedNamespaces() = [| |]
      member this.GetTypes() = [| typeof<RouteProvider> |]

[<assembly: TypeProviderAssembly>]
do ()
