#r @"..\..\..\RouteProvider\bin\Debug\RouteProvider.dll"

[<Literal>]
let routes = """
  GET projects/{projectId} as getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId:int} as updateProject
  GET projects/statistics
  GET people/{name:string} as getPerson
"""

type MyRoutes = IsakSky.RouteProvider<routes>

let router = MyRoutes(
              getProject = (fun projectId -> printfn "You asked for project %d" projectId),
              getProjectComments = (fun projectId commentId ->
                                                                    printfn "You asked for project %d and comment %d" projectId commentId),
              updateProject = (fun p -> printfn "Updated project %d" p),
              // If you don't provide a route name, one will be computed for you
              GET__projects_statistics = (fun () -> printfn "You asked for project statistics"),
              getPerson = (fun name -> printfn "You asked for a person called \"%s\"" name))

MyRoutes.Builders.getProject(1L)
MyRoutes.Builders.getProjectComments(1L, 123L)
router.DispatchRoute("GET", "/projects/123/")
//router.DispatchRoute("ctx", "GET", "/projects/123/comments/4/")