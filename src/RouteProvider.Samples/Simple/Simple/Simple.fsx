#r @"..\..\..\RouteProvider\bin\Debug\RouteProvider.dll"

[<Literal>]
let routes = """
  GET projects/{projectId} as getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId} as updateProject
  GET projects/statistics
"""

type MyRoutes = IsakSky.RouteProvider<routes>

let router = MyRoutes(
              getProjectHandler = (fun projectId -> printfn "You asked for project %d" projectId),
              getProjectCommentsHandler = (fun projectId commentId ->
                                                                    printfn "You asked for project %d and comment %d" projectId commentId),
              updateProjectHandler = (fun p -> printfn "Updated project %d" p),
              // If you don't provide a route name, one will be computed for you
              GET__projects_statisticsHandler = (fun () -> printfn "You asked for project statistics")
             )

MyRoutes.Builders.getProject(1L)
MyRoutes.Builders.getProjectComments(1L, 123L)
router.DispatchRoute("GET", "/projects/123")
router.DispatchRoute("GET", "/projects/123/comments/4/")