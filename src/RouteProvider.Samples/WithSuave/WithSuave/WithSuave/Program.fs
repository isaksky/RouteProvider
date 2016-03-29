// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Suave
open Suave.Http
open Suave.Operators
open Suave.Successful
open Suave.Logging

open IsakSky

type HttpMethod with
  member this.Name =
    match this with
    | HttpMethod.GET -> "GET"
    | HttpMethod.POST -> "POST"
    | HttpMethod.DELETE -> "DELETE"
    | HttpMethod.PUT -> "PUT"
    | HttpMethod.HEAD -> "HEAD"
    | HttpMethod.CONNECT -> "CONNECT"
    | HttpMethod.PATCH -> "PATCH"
    | HttpMethod.TRACE -> "TRACE"
    | HttpMethod.OPTIONS -> "OPTIONS"
    | HttpMethod.OTHER(name) -> name

[<Literal>]
let routes = """
  GET projects/{projectId} as getProject
  PUT projects/{projectId} as updateProject
  GET projects/{projectName:string}/comments/{commentId} as getProjectComments
"""

[<Literal>]
let outputPath = __SOURCE_DIRECTORY__ + "\MyRoutes.fs"

type Dummy = RouteProvider<"MyRoutes", routes, true, true, outputPath>
open MyNamespace.MyModule

let router = {
  getProject = fun (ctx:HttpContext) projectId ->
    ctx |> OK(sprintf "Get project %d" projectId)
  updateProject = fun ctx projectId ->
    ctx |> OK(sprintf "Update project %d" projectId)
  getProjectComments = fun ctx projectName commentId ->
    ctx |> OK(sprintf "GET Project \"%s\" / Comment %d" projectName commentId)
  notFound = Some <| (fun ctx verb path ->
    (RequestErrors.NOT_FOUND "Not found") ctx)}

let dispatchRouter : WebPart =
  fun ctx ->
    async {
      return! router.DispatchRoute(ctx, ctx.request.``method``.Name, ctx.request.url)
    }

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    let cfg = { defaultConfig with logger = Loggers.ConsoleWindowLogger LogLevel.Verbose}
    startWebServer defaultConfig dispatchRouter
    0
