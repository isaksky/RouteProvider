module Program

[<Literal>]
let routes2 = """
  GET projects/{projectId} as getProject
  PUT projects/{foo:string} as updateProject
  POST projects/{projectId:int} as createProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
"""
[<Literal>]
let outputPath = __SOURCE_DIRECTORY__ + "\MyRoutes.fs"

type Dummy = IsakSky.RouteProvider<"MyRoutes",routes2, false, false, outputPath>

open MyNamespace.MyModule

let router : MyRoutes =
  { getProject = fun p -> printfn "Hi project %d" p
    updateProject = fun ps -> printfn "Hi project string %s" ps
    getProjectComments = fun p c -> printfn "Hi project comment %d %d" p c
    createProject = fun p -> printfn "Creating project %d" p
    notFound = None }
