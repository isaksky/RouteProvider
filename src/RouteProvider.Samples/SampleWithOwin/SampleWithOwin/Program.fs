open System
open System.IO
open Owin
open Microsoft.Owin
open System.Threading.Tasks
open Microsoft.Owin.Hosting
open Microsoft.Owin.Host.HttpListener

[<Literal>]
let routes = """
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  GET projects/{projectId} as getProject  
"""

type Routes = IsakSky.RouteProvider<routes, "Microsoft.Owin.IOwinContext">

let router = Routes(getProjectCommentsHandler = (fun ctx projectId commentId -> (
                                                  ctx.Response.Write(sprintf "You asked for project %d and comment %d" projectId commentId)
                                                )),
                    getProjectHandler = (fun ctx projectId -> 
                                          ctx.Response.Write("You asked for a project")))

type WebApp() =
  let handleOwinContext (ctx:IOwinContext) =
    try 
      router.DispatchRoute(ctx, ctx.Request.Method, ctx.Request.Path.ToString())
    with 
    | :? System.Exception as ex ->
      printfn "Got exception %A" ex
 
  let owinHandler = fun (context:IOwinContext) (_:Func<Task>) -> 
    handleOwinContext context;
    Task.FromResult(null) :> Task;

  member x.Configuration (app:IAppBuilder) = 
    app.Use(owinHandler) |> ignore

[<assembly: OwinStartup(typeof<WebApp>)>]
do ()

[<EntryPoint>]
let main argv = 
    let url = "http://*:4567"
    
    try
      printfn "Starting up..."
      use app = WebApp.Start<WebApp>(url)
      printfn "Listening on %s" url
      Console.ReadLine() |> ignore
      0
    with
      | :? System.Net.HttpListenerException as ex ->
        let cmdStr = @"netsh http add urlacl url=""http://*:4567/"" delegate=yes user=everyone"
        printfn "Access denied. Run this command in an admin elevated terminal: %A" cmdStr
        1