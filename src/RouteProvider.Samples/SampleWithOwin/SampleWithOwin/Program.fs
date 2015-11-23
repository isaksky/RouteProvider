open System
open System.IO
open Owin
open Microsoft.Owin
open System.Threading.Tasks
open Microsoft.Owin.Hosting
open Microsoft.Owin.Host.HttpListener
type A = Owin.IAppBuilder list

[<Literal>]
let routes = """
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId} as updateProject  
"""

type Routes = IsakSky.RouteProvider<routes, "Owin.IAppBuilder">

let router = Routes(getProjectCommentsHandler = (fun ctx projectId commentId -> ()),
                    updateProjectHandler = (fun ctx projectId -> ()))

type WebApp() =
  let handleOwinContext (ctx:IOwinContext) =
    use writer = new StreamWriter(ctx.Response.Body)
    match ctx.Request.Path.Value with
    | "/Hello" ->
      ctx.Response.Write "cool"
    | _ -> ctx.Response.Write "Something"
 
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