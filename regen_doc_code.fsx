#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
#if MONO
#else
#load "packages/SourceLink.Fake/tools/Fake.fsx"
open SourceLink
#endif

//#r @"bin\RouteProvider\RouteProvider.dll"
#r @"src\RouteProvider\bin\Debug\RouteProvider.dll"
open IsakSky.RouteProvider.RouteCompilation
open IsakSky.RouteProvider.RouteParsing

let routes = """
  GET projects/{projectId} as getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId:int} as updateProject
  GET projects/statistics
  GET people/{name:string} as getPerson
"""

let genSample (opts:RouteCompilationArgs) =
  let argStrs = [
    sprintf "%A" (opts.typeName)
    "routes"
    sprintf "%A" (opts.inputType)
    sprintf "%A" (opts.returnType)
    "\"MyRoutes.fs\""
  ]

  let argStr = argStrs |> String.concat ", "
  let tpCall = sprintf "type Dummy = IsakSky.RouteProvider<%s>" argStr
  let cfg = sprintf "Configuration:\n\n    %s\n" tpCall

  let sw = new StringWriter()
  compileRoutes opts sw
  let code = sw.ToString()
  let codelines = code.Split([|"\r\n"; "\r"; "\n"|], (StringSplitOptions.None))
  let description =
    match opts.inputType, opts.returnType with
    | true, true ->
      "Generated code with input and return type"
    | true, false ->
      "Generated code with input type"
    | false, false ->
      "Generated code"
    | _ -> failwith "Logic error"

  let offsetcode =
    codelines
    |> Seq.map (fun s -> sprintf "%s\n    " s)
    |> String.concat ""
  code, sprintf "%s\n\n%s:\n\n    [lang=fsharp]\n    %s" cfg description offsetcode

match parseRoutes(routes) with
| Success(routes) ->
  let opts1 =
    { typeName = "MyRoutes"
      inputType = false
      returnType = false
      parse = routes
      nameSpace = None
      moduleName = None
      outputType = FSharp}
  let targetDir = "docs/content"
  let targetFile1 = targetDir @@ "notypes.md"
  ensureDirectory targetDir
  System.IO.File.WriteAllText(targetFile1, (genSample opts1) |> snd)

  let opts2 =
    { opts1 with inputType = true }
  let targetFile2 = targetDir @@ "input_type.md"
  System.IO.File.WriteAllText(targetFile2, (genSample opts2) |> snd)

  let opts3 =
    { opts2 with
        returnType = true }
  let targetFile3 = targetDir @@ "input_and_return_type.md"
  let targetCodeFile3 = targetDir @@ "input_and_return_type.fs"
  ensureDirectory targetDir
  let code, samp = genSample opts3
  System.IO.File.WriteAllText(targetCodeFile3, code)
  System.IO.File.WriteAllText(targetFile3, samp)
| Failure(err) -> failwithf "Failed to parse routes: %s" err

let routes2 = """
  GET projects/{projectId} as getProject
  PUT projects/{foo:string}
  POST projects/{projectId:int} as createProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
"""

match parseRoutes(routes2) with
| Success(routes) ->
  let opts1 =
    { typeName = "MyRoutes"
      inputType = false
      returnType = false
      parse = routes
      nameSpace = None
      moduleName = None
      outputType = FSharp}
  let targetDir = "docs/content"
  let targetFile = targetDir @@ "simple.fs"
  ensureDirectory targetDir
  let code, _ = genSample opts1
  System.IO.File.WriteAllText(targetFile, code)

| Failure(err) -> failwithf "Failed to parse routes: %s" err
