# RouteProvider

An F# type provider for routes.

``` Fsharp
[<Literal>]
let routes = """
  GET projects/{projectId}
  PUT projects/{projectId}/close  
"""

type Routes = IsakSky.RouteProvider<routes>
let getProject123 = Routes.``GET projects/{projectId}`` 123L
```

And then:

``` Fsharp
getProject123.projectId // val it : int64 = 123L
```
