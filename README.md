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

## Todo
- Support other types of path segments than just ```int64```
- Add a top level property with a list of route descriptions, for use in matching
- Provide a sample matching function (e.g., ```fun (path:string) -> Route option```)
