// Generated by RouteProvider 0.0.0.0
namespace MyNamespace

open System
module MyModule =
  let getProjectComments (project:int64) (comment:int64) =
      "projects/" + project.ToString() + "comments/" + comment.ToString()
  let putProjectComments (project:int64) =
      "projects/" + project.ToString() + "comments/"

  module Internal =
    type TryParseState =
    | Untried = 0
    | Success = 1
    | Failed = 2

    let tryParseInt32 (s:string, parseState: TryParseState byref, result: int byref) =
      match parseState with
      | TryParseState.Failed
      | TryParseState.Success -> ()
      | _ ->
        parseState <- match Int32.TryParse(s, &result) with
        | true -> TryParseState.Success
        | false -> TryParseState.Failed
      parseState = TryParseState.Success

    let tryParseInt64 (s:string, parseState: TryParseState byref, result: int64 byref) =
      match parseState with
      | TryParseState.Failed
      | TryParseState.Success -> ()
      | _ ->
        parseState <- match Int64.TryParse(s, &result) with
        | true -> TryParseState.Success
        | false -> TryParseState.Failed
      parseState = TryParseState.Success

    let fakeBaseUri = new Uri("http://a.a")

    exception RouteNotMatchedException of string * string

  type MyRoutes<'TContext, 'TReturn> =
    { getProjectComments: 'TContext->int64->int64->'TReturn
      putProjectComments: 'TContext->int64->'TReturn
      notFound: ('TContext->string->string->'TReturn) option }

    member inline private this.HandleNotFound(context, verb, path) =
      match this.notFound with
      | None -> raise (Internal.RouteNotMatchedException (verb, path))
      | Some(notFound) -> notFound context verb path

    member this.DispatchRoute(context:'TContext, verb:string, path:string) : 'TReturn =
      let parts = path.Split('/')
      let start = if parts.[0] = "" then 1 else 0
      let endOffset = if parts.Length > 0 && parts.[parts.Length - 1] = "" then 1 else 0
      match parts.Length - start - endOffset with
      | 4 ->
        if String.Equals(parts.[0 + start],"projects") then
          let mutable project = 0L
          let mutable project_parseState = Internal.TryParseState.Untried
          if Internal.tryParseInt64(parts.[1 + start], &project_parseState, &project) then
            if String.Equals(parts.[2 + start],"comments") then
              let mutable comment = 0L
              let mutable comment_parseState = Internal.TryParseState.Untried
              if Internal.tryParseInt64(parts.[3 + start], &comment_parseState, &comment) then
                // endPointScope
                // [Int64Param "project"; Int64Param "comment"]
                // scope1
                // [(3, Int64Param "comment"); (1, Int64Param "project")]
                // scope2
                // [(1, Int64Param "project"); (3, Int64Param "comment")]
                if verb = "GET" then this.getProjectComments context project comment
                else this.HandleNotFound(context, verb, path)
              else this.HandleNotFound(context, verb, path)
            else this.HandleNotFound(context, verb, path)
          else this.HandleNotFound(context, verb, path)
        else this.HandleNotFound(context, verb, path)
      | 3 ->
        if String.Equals(parts.[0 + start],"projects") then
          let mutable project = 0L
          let mutable project_parseState = Internal.TryParseState.Untried
          if Internal.tryParseInt64(parts.[1 + start], &project_parseState, &project) then
            if String.Equals(parts.[2 + start],"comments") then
              // endPointScope
              // [Int64Param "project"]
              // scope1
              // [(1, Int64Param "project")]
              // scope2
              // [(1, Int64Param "project")]
              if verb = "POST" then this.putProjectComments context project
              else this.HandleNotFound(context, verb, path)
            else this.HandleNotFound(context, verb, path)
          else this.HandleNotFound(context, verb, path)
        else this.HandleNotFound(context, verb, path)
      | _ ->
        this.HandleNotFound(context, verb, path)

    member this.DispatchRoute(context:'TContext, verb:string, uri:Uri) : 'TReturn =
      // Ensure we have an Absolute Uri, or just about every method on Uri chokes
      let uri = if uri.IsAbsoluteUri then uri else new Uri(Internal.fakeBaseUri, uri)
      let path = uri.GetComponents(UriComponents.Path, UriFormat.Unescaped)
      this.DispatchRoute(context, verb, path)
