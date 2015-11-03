# RouteProvider

This is an F# Type provider made to generate types suitable for routing in a web application.

## Example: 

``` Fsharp
[<Literal>]
let routes = """
  GET projects/{projectId} AS getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId} as updateProject
"""

type MyRoutes = IsakSky.Routes<routes>

let router = MyRoutes(
              getProjectHandler = (fun projectId -> printfn "You asked for project %d" projectId),
              getProjectCommentsHandler = (fun projectId commentId ->
                                                                    printfn "You asked for project %d and comment %d" projectId commentId),
              updateProjectHandler = (fun p -> printfn "Updated project %d" p)
             )
```

Now we can use the router like this:

    router.dispatchRoute("GET", "projects/4321/comments/1234")
    -> "You asked for project 4321 and comment 1234"

## Todo
- Option for user type that will passed to handler functions
- Option for user type that handler functions must return
- Support other types of path segments than just ```int64```
- Allow routes to be defined in a seperate file
- Implement ToString() of generated route types

## How does it work?

It generates CSharp types. For example, for the routes defined above, it generates the following CSharp:

```CSharp
using System;
namespace IsakSky {
  public class MyRoutes {
    static bool StringIsAllDigits(string s){
      foreach (char c in s){
        if (c < '0' || c > '9') { return false; }
      }
      return true;
    }
    public class getProject {
      public readonly long projectId;
    }
    public class getProjectComments {
      public readonly long projectId;
      public readonly long commentId;
    }
    public class updateProject {
      public readonly long projectId;
    }
    public MyRoutes(
      Action<long> getProjectHandler,
      Action<long, long> getProjectCommentsHandler,
      Action<long> updateProjectHandler) {
        this.getProjectHandler = getProjectHandler;
        this.getProjectCommentsHandler = getProjectCommentsHandler;
        this.updateProjectHandler = updateProjectHandler;
      }
    public readonly Action<long> getProjectHandler;
    public readonly Action<long, long> getProjectCommentsHandler;
    public readonly Action<long> updateProjectHandler;
    public void DispatchRoute(string verb, string path) {
      var parts = path.Split('/');
      var start = 0;
      if (parts[0] == "") { start = 1; }
      var endOffset = parts[parts.Length - 1] == "" ? 1 : 0;
      switch (parts.Length - start - endOffset) {
        case 2:
          if (parts[start + 0] == "projects"){
            if (StringIsAllDigits(parts[start + 1])){
              var projectId = long.Parse(parts[start + 1]);
              if (verb == "PUT") { this.updateProjectHandler(projectId); return; }
              else if (verb == "GET") { this.getProjectHandler(projectId); return; }
            }
          }
          break;
        case 4:
          if (parts[start + 0] == "projects"){
            if (StringIsAllDigits(parts[start + 1])){
              var projectId = long.Parse(parts[start + 1]);
              if (parts[start + 2] == "comments"){
                if (StringIsAllDigits(parts[start + 3])){
                  var commentId = long.Parse(parts[start + 3]);
                  if (verb == "GET") { this.getProjectCommentsHandler(projectId, commentId); return; }
                }
              }
            }
          }
          break;
        default: throw new ArgumentException();
      }
    }
  }
}
```

