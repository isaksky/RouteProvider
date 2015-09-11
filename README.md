# RouteProvider

Static typing. Routes. How to?

## Example: 
``` Fsharp
[<Literal>]
let routes = """
  GET projects/{projectId} 
  GET projects/{projectId}/comments/{commentId}
  PUT projects/{projectId} 
"""

type Routes = IsakSky.RouteProvider<routes>
```
Now we have created a Routes type with 3 subtypes (one for each route). The Route types are named like this:

``` Fsharp
Routes.``GET projects/{projectId}``
```

The names of the route types cannot be changed by design. We keep the static information for each route around. For example, the verb:

``` Fsharp
Routes.``PUT projects/{projectId}/close``.verb // val it : string = "PUT"
```

And the route segments:

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

We can also make a router by instantiating the Routes type:

``` Fsharp
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
    
Note: if a matching route is not found, we throw an exception, unless a notFound handler is provided while instantiating the router, in which case that is called.

## Todo
- Stop the dynamic dispatch (perf killer)
- Allow a user arg to the RouteProvider, for passing to the handler functions
- Support other types of path segments than just ```int64```
- Add a concise way to do define multiple verbs per route
- Allow routes to be defined in a seperate file
- Configurable return type from handler functions?
