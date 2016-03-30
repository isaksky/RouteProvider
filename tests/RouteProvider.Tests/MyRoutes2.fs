// Generated by RouteProvider 0.0.0.0
namespace Ns2

open System
module MyModule =
  let getProjStats  =
      "projects/statistics/"
  let getProjAction (action:string) =
      "projects/" + action

  module Internal =
    let fakeBaseUri = new Uri("http://a.a")

    exception RouteNotMatchedException of string * string

  type MyRoutes2 =
    { getProjStats: unit->unit
      getProjAction: string->unit
      notFound: (string->string->unit) option }

    member inline private this.HandleNotFound(verb, path) =
      match this.notFound with
      | None -> raise (Internal.RouteNotMatchedException (verb, path))
      | Some(notFound) -> notFound verb path

    member this.DispatchRoute(verb:string, path:string) : unit =
      let parts = path.Split('/')
      let start = if parts.[0] = "" then 1 else 0
      let endOffset = if parts.Length > 0 && parts.[parts.Length - 1] = "" then 1 else 0
      match parts.Length - start - endOffset with
      | 2 ->
        if String.Equals(parts.[0 + start],"projects") then
          if String.Equals(parts.[1 + start],"statistics") then
            if verb = "GET" then this.getProjStats()
            else this.HandleNotFound(verb, path)
          else
            if verb = "GET" then this.getProjAction (parts.[1 + start])
            else this.HandleNotFound(verb, path)
        else this.HandleNotFound(verb, path)
      | _ ->
        this.HandleNotFound(verb, path)

    member this.DispatchRoute(verb:string, uri:Uri) : unit =
      // Ensure we have an Absolute Uri, or just about every method on Uri chokes
      let uri = if uri.IsAbsoluteUri then uri else new Uri(Internal.fakeBaseUri, uri)
      let path = uri.GetComponents(UriComponents.Path, UriFormat.Unescaped)
      this.DispatchRoute(verb, path)