open System.Net
open System.Diagnostics
open System
open IsakSky

[<Literal>]
let routes = """
  GET projects/{projectId} as getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
"""

type MyRoutes = RouteProvider<routes, "System.Net.HttpListenerContext", "string">

let router = 
  MyRoutes(
    getProject = (fun ctx projectId ->
      let path = ctx.Request.RawUrl
      sprintf "Here is project %d" projectId),
    getProjectComments = fun ctx _ commentId -> 
      sprintf "Here is comment %d" commentId)

[<EntryPoint>]
let main argv = 
    let listener = new HttpListener()
    listener.Prefixes.Add("http://localhost:8080/")
    listener.Start()
    Console.WriteLine("Listening...")
    for idx in [0..5] do
      let context = listener.GetContext()
      try 
        let resp = router.DispatchRoute(context, context.Request.HttpMethod, context.Request.RawUrl)
        let buffer = System.Text.Encoding.UTF8.GetBytes(resp)
        let response = context.Response
        response.ContentLength64 <- (int64)buffer.Length
        let output = response.OutputStream
        output.Write(buffer, 0, buffer.Length)
        output.Close()
      with
      | :? MyRoutes.RouteNotMatchedException as ex ->
        printfn "Route not matched! Method: \"%s\" Path: \"%s\"" ex.Verb ex.Path
      
    Console.ReadLine() |> ignore
    listener.Stop()
    printfn "%A" argv

    0 // return an integer exit code
