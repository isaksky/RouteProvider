﻿open System
open System.IO
open Owin
open Microsoft.Owin
open System.Threading.Tasks
open Microsoft.Owin.Hosting
open Microsoft.Owin.Host.HttpListener

open IsakSky.RouteProvider

[<Literal>]
let routes = """
  GET  projects/{project}/comments/{comment} as getProjectComments
  POST projects/{project}/comments as putProjectComments
"""

let [<Literal>] outputPath = __SOURCE_DIRECTORY__ + "\MyRoutes.fs"

open MyNamespace.MyModule

type Dummy = IsakSky.RouteProvider<"MyRoutes", routes, true, true, outputPath>

//let router = Routes(getProjectComments = (fun ctx projectId commentId ->
//  ctx.Response.Write(sprintf "You asked for project %d and comment %d" projectId commentId)),
//                    putProjectComments = (fun ctx projectId ->
//                                          ctx.Response.Write(sprintf "You asked for project %d" projectId)))

let router =
  { getProjectComments = fun (ctx:IOwinContext) projectId commentId ->
      ctx.Response.Write(sprintf "You asked for project %d and comment %d" projectId commentId)
    putProjectComments = fun ctx projectId ->
      ctx.Response.Write(sprintf "You asked for project %d" projectId)
    notFound = None}

type WebApp() =
  let handleOwinContext (ctx:IOwinContext) =
    try
      router.DispatchRoute(ctx, ctx.Request.Method, ctx.Request.Path.ToString())
    with
    | MyNamespace.MyModule.Internal.RouteNotMatchedException(verb, path) ->
      printfn "Route %s %s not matched" verb path

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
