# RouteProvider

Static typing. Routes. How to?

## Example: 
``` Fsharp
[<Literal>]
let routes = """
  GET projects/{projectId}
  PUT projects/{projectId}/close  
"""
type Routes = IsakSky.RouteProvider<routes>
```
Now we have defined 2 Route types. The Route types are named like this:

``` Fsharp
Routes.``GET projects/{projectId}``
```
We keep the static information for each Type around:
``` Fsharp
// Get the verb
Routes.``PUT projects/{projectId}/close``.verb // val it : string = "PUT"
```
``` Fsharp
// Get the route segments
Routes.``PUT projects/{projectId}/close``.routeSegments 
(* val it : ProviderImplementation.PathSegment [] =
  [|ProviderImplementation.ConstantSeg {name = "projects";};
    ProviderImplementation.Int64Seg {name = "projectId";};
    ProviderImplementation.ConstantSeg {name = "close";}|] *)
```
We can make instances of these routes:
``` Fsharp
// Make an instance to represent getting project with id 123
let getProject123 = Routes.``GET projects/{projectId}`` 123L
// Provided arguments are accessible by name
getProject123.projectId // val it : int64 = 123L
```

``` Fsharp
[<Literal>]
let routes = """
  GET projects/{projectId} 
  GET projects/{projectId}/comments/{commentId}
  PUT projects/{projectId} 
"""

type Routes = IsakSky.RouteProvider<routes>

let router = Routes(
              ``GET projects/{projectId}`` = (fun projectId -> printfn "You asked for project %d" projectId),
              ``GET projects/{projectId}/comments/{commentId}`` = (fun projectId commentId ->
                                                                    printfn "You asked for project %d and comment %d" projectId commentId),
              ``PUT projects/{projectId}`` = (fun p -> printfn "Update project %d" p)
             )
```

Now we can use the router like this:

    router.dispatchRoute("GET", "projects/4321/comments/1234")
    -> "You asked for project 4321 and comment 1234"

## Todo
- Support other types of path segments than just ```int64```
- Not found handlers
- Add a concise way to do define multiple verbs per route
- Allow routes to be defined in a seperate file
