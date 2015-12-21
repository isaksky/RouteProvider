namespace FSharpWeb3

open System
open System.Net.Http
open System.Web
open System.Web.Http
open System.Web.Mvc
open System.Web.Routing
open System.Web.Optimization
open System.Net
open IsakSky.RouteProvider
open System.Threading.Tasks

module Routing =
  [<Literal>]
  let routes = """
    GET projects/{projectId} as getProject
  """
  type Routes = 
    IsakSky.RouteProvider<
      routes, 
      "System.Net.Http.HttpRequestMessage", // Input type name
      "System.Threading.Tasks.Task<System.Net.Http.HttpResponseMessage>"> // Return type name

  let router =
    Routes(
      getProject = (fun ctx projId ->
        async {
          let msg = sprintf "You asked for proj %d" projId
          let resp = new HttpResponseMessage()
          resp.Content <- new StringContent(msg)
          return resp } |> Async.StartAsTask),
      notFound = (fun ctx verb path ->
        async {
          let msg = sprintf "Route \"%s\" \"%s\" not found" verb path
          let resp = new HttpResponseMessage()
          resp.Content <- new StringContent(msg)
          return resp } |> Async.StartAsTask))

module Internal =
  let globalHandler = 
    { new HttpMessageHandler() with 
        override this.SendAsync(request, cancellationToken) : Task<HttpResponseMessage> =
          Routing.router.DispatchRoute(request, request.Method.Method, request.RequestUri.AbsolutePath)}

type Global() =
    inherit System.Web.HttpApplication()
    static member RegisterWebApi(config: HttpConfiguration) =
        config.Routes.MapHttpRoute(
          "The Right Thing (TM)", 
          "{*url}", 
          null, 
          null, 
          (Internal.globalHandler)
           ) |> ignore

    static member RegisterFilters(filters: GlobalFilterCollection) =
        filters.Add(new HandleErrorAttribute())

    member x.Application_Start() =
        AreaRegistration.RegisterAllAreas()
        GlobalConfiguration.Configure(Action<_> Global.RegisterWebApi)
        Global.RegisterFilters(GlobalFilters.Filters)
