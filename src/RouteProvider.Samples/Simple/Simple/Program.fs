module Program

[<Literal>]
let routes2 = """
  GET projects/{projectId} as getProject
  PUT projects/{foo:string} as updateProject
  POST projects/{projectId:Guid} as createProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
"""
[<Literal>]
let outputPath = __SOURCE_DIRECTORY__ + "\MyRoutes.fs"

type Dummy = IsakSky.RouteProvider<"MyRoutes",routes2, false, false, outputPath, "Hello">

open Hello.MyModule

let router : MyRoutes =
  { getProject = fun p -> printfn "Hi project %d" p
    updateProject = fun ps -> printfn "Hi project string %s" ps
    getProjectComments = fun p c -> printfn "Hi project comment %d %d" p c
    createProject = fun p -> printfn "Creating project %A" p
    notFound = None }

let router2 =
  MyRoutes.Router(
    getProject = (fun p -> printfn "P %d" p),
    updateProject = (fun ps -> printfn "hi %s" ps),
    createProject = (fun p -> printfn "Create project %A" p),
    getProjectComments = (fun p c -> printfn "Hi %d %d" p c))
