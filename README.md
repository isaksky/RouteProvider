# RouteProvider

An F# Type provider that generates types suitable for routing in a web application.

## Example: 

``` Fsharp
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
```

You can use ```int64```, ```int```, or ```string``` as type annotations. The default is ```int64```.

Now we can use the router like this:

    router.dispatchRoute("GET", "projects/4321/comments/1234")
    -> "You asked for project 4321 and comment 1234"

You can also build paths in a typed way like this:

    let url = MyRoutes.Builders.getProjectComments(123L,4L)
    -> "/projects/123/comments/4"
    
To integrate with the web library you are using, you can pass in a fully qualified type name as the second argument:

    type Routes = IsakSky.RouteProvider<routes, "Microsoft.Owin.IOwinContext">
    
The generated dispatch and handler functions will then take that type as the first argument. You can also specify a type name as the third argument, which will make the dispatch function have a return type, and will require all of you handlers to return that type.

## Example

Example with both input and return types specified:

![Example](/demo.png?raw=true "Example")

## Brief demo in visual studio:

https://www.youtube.com/watch?v=r7cdeTzPY58

## Roadmap / planned features
- Allow routes to be defined in a seperate file
- Add support for Guids

## Comparison with other approaches

| Project         | Route definition mechanism                             | Bidirectional? | Type safety   |
|-----------------|:-------------------------------------------------------|:---------------|:--------------|
| ASP.NET MVC     | Reflection on attributes and method naming conventions | No             | Limited       |
| Freya           | Uri Templates                                          | Yes            | None          | 
| Suave.IO        | F# sprintf format string                               | No             | Yes           |
| bidi (Clojure)  | Data                                                   | Yes            | None          |
| Ruby on Rails   | Internal Ruby DSL                                      | Yes            | None          |
| Yesod (Haskell) | Types generated from Route DSL via Template Haskell    | Yes            | Full          |
| RouteProvider   | Types generated from Route DSL via #F Type Provider    | Yes            | Full          |

## Installation

You can install it via Nuget:

```Install-Package RouteProvider -Pre```

## How does it work?

It generates FSharp code. For example, for the routes defined above, it generates the following FSharp:

```FSharp
namespace IsakSky
open System
module RouteProvider =
  let getProject (projectId:int64) =
      "projects/" + projectId.ToString()
  let getProjectComments (projectId:int64) (commentId:int64) =
      "projects/" + projectId.ToString() + "comments/" + commentId.ToString()
  let updateProject (projectId:int) =
      "projects/" + projectId.ToString()
  let GET__projects_statistics  =
      "projects/statistics/"
  let getPerson (name:string) =
      "people/" + name

  module Internal =
    let stringIsAllDigits (s:string) =
      let mutable i = 0
      let mutable foundNonDigit = false
      while i < s.Length && not foundNonDigit do
        let c = s.[i]
        if c < '0' || c > '9' then foundNonDigit <- true
        i <- i + 1
      not foundNonDigit
    exception RouteNotMatchedException of string * string

  type MyRoutes<'TContext, 'TReturn> =
    { getProject: 'TContext->int64->'TReturn
      getProjectComments: 'TContext->int64->int64->'TReturn
      updateProject: 'TContext->int->'TReturn
      GET__projects_statistics: 'TContext->'TReturn
      getPerson: 'TContext->string->'TReturn
      notFound: ('TContext->string->string->'TReturn) option }

    member this.DispatchRoute (context:'TContext) (verb:string) (path:string) : 'TReturn =
      let parts = path.Split('/')
      let start = if parts.[0] = "" then 1 else 0
      let endOffset = if parts.Length > 0 && parts.[parts.Length - 1] = "" then 1 else 0
      match parts.Length - start - endOffset with
      | 2 ->
        if parts.[start + 0] = "people" then (* hi *)
          let name = parts.[start + 1]
          if verb = "GET" then (* ho *) 
            this.getPerson context name
          else
            match this.notFound with
            | None -> raise (Internal.RouteNotMatchedException (verb, path))
            | Some(notFound) -> notFound context verb path

        elif parts.[start + 0] = "projects" then (* hi *)
          if parts.[start + 1] = "statistics" then (* hi *)
            if verb = "GET" then (* ho *) 
              this.GET__projects_statistics context
            else
              match this.notFound with
              | None -> raise (Internal.RouteNotMatchedException (verb, path))
              | Some(notFound) -> notFound context verb path

          elif Internal.stringIsAllDigits(parts.[start + 1]) then (* hi *)
            let projectId = Int32.Parse(parts.[start + 1])
            if verb = "PUT" then (* ho *) 
              this.updateProject context projectId
            else
              match this.notFound with
              | None -> raise (Internal.RouteNotMatchedException (verb, path))
              | Some(notFound) -> notFound context verb path

          elif Internal.stringIsAllDigits(parts.[start + 1]) then (* hi *)
            let projectId = Int64.Parse(parts.[start + 1])
            if verb = "GET" then (* ho *) 
              this.getProject context projectId
            else
              match this.notFound with
              | None -> raise (Internal.RouteNotMatchedException (verb, path))
              | Some(notFound) -> notFound context verb path

          else
            match this.notFound with
            | None -> raise (Internal.RouteNotMatchedException (verb, path))
            | Some(notFound) -> notFound context verb path

        else
          match this.notFound with
          | None -> raise (Internal.RouteNotMatchedException (verb, path))
          | Some(notFound) -> notFound context verb path
      | 4 ->
        if parts.[start + 0] = "projects" then (* ho *) 
          if Internal.stringIsAllDigits(parts.[start + 1]) then (* ho *) 
            let projectId = Int64.Parse(parts.[start + 1])
            if parts.[start + 2] = "comments" then (* ho *) 
              if Internal.stringIsAllDigits(parts.[start + 3]) then (* ho *) 
                let commentId = Int64.Parse(parts.[start + 3])
                if verb = "GET" then (* ho *) 
                  this.getProjectComments context projectId commentId
                else
                  match this.notFound with
                  | None -> raise (Internal.RouteNotMatchedException (verb, path))
                  | Some(notFound) -> notFound context verb path

              else
                match this.notFound with
                | None -> raise (Internal.RouteNotMatchedException (verb, path))
                | Some(notFound) -> notFound context verb path

            else
              match this.notFound with
              | None -> raise (Internal.RouteNotMatchedException (verb, path))
              | Some(notFound) -> notFound context verb path

          else
            match this.notFound with
            | None -> raise (Internal.RouteNotMatchedException (verb, path))
            | Some(notFound) -> notFound context verb path

        else
          match this.notFound with
          | None -> raise (Internal.RouteNotMatchedException (verb, path))
          | Some(notFound) -> notFound context verb path
      | _ ->
        match this.notFound with
        | None -> raise (Internal.RouteNotMatchedException (verb, path))
        | Some(notFound) -> notFound context verb path

```

