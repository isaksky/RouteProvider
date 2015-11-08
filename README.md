# RouteProvider

This is an F# Type provider made to generate types suitable for routing in a web application.

## Example: 

``` Fsharp
[<Literal>]
let routes = """
  GET projects/{projectId} as getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId} as updateProject
  GET projects/statistics
"""

type MyRoutes = IsakSky.RouteProvider<routes>

let router = MyRoutes(
              getProjectHandler = (fun projectId -> printfn "You asked for project %d" projectId),
              getProjectCommentsHandler = (fun projectId commentId ->
                                                                    printfn "You asked for project %d and comment %d" projectId commentId),
              updateProjectHandler = (fun p -> printfn "Updated project %d" p),
              // If you don't provide a route name, one will be computed for you
              GET__projects_statisticsHandler = (fun () -> printfn "You asked for project statistics")
             )
```

Now we can use the router like this:

    router.dispatchRoute("GET", "projects/4321/comments/1234")
    -> "You asked for project 4321 and comment 1234"

You can also build paths in a typed way like this:

    let url = MyRoutes.Builders.getProjectComments(123L,4L)
    -> "/projects/123/comments/4"

## Todo
- Option for user type that will passed to handler functions (e.g., OwinContext)
- Option for user type that handler functions must return (e.g., HttpWebResponse)
- Support other types of path segments than just ```int64```
- Allow routes to be defined in a seperate file

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

## How does it work?

It generates CSharp types. For example, for the routes defined above, it generates the following CSharp:

```CSharp
using System;
namespace IsakSky {
  public class MyRoutes {
    public class getProject {
      public long projectId;
      public override string ToString() {
        return string.Format("/projects/{0}", this.projectId);
      }
    }
    public class getProjectComments {
      public long projectId;
      public long commentId;
      public override string ToString() {
        return string.Format("/projects/{0}/comments/{1}", this.projectId, this.commentId);
      }
    }
    public class updateProject {
      public long projectId;
      public override string ToString() {
        return string.Format("/projects/{0}", this.projectId);
      }
    }
    public class GET__projects_statistics {
      public override string ToString() {
        return "/projects/statistics";
      }
    }
    public MyRoutes(
      Action<long> getProjectHandler,
      Action<long, long> getProjectCommentsHandler,
      Action<long> updateProjectHandler,
      Action GET__projects_statisticsHandler) {
        this.getProjectHandler = getProjectHandler;
        this.getProjectCommentsHandler = getProjectCommentsHandler;
        this.updateProjectHandler = updateProjectHandler;
        this.GET__projects_statisticsHandler = GET__projects_statisticsHandler;
      }
    public readonly Action<long> getProjectHandler;
    public readonly Action<long, long> getProjectCommentsHandler;
    public readonly Action<long> updateProjectHandler;
    public readonly Action GET__projects_statisticsHandler;

    public void DispatchRoute(string verb, string path) {
      var parts = path.Split('/');
      var start = 0;
      if (parts[0] == "") { start = 1; }
      var endOffset = parts.Length > 0 && parts[parts.Length - 1] == "" ? 1 : 0;
      switch (parts.Length - start - endOffset) {
        case 2:
          if (parts[start + 0] == "projects"){
            if (parts[start + 1] == "statistics"){
              if (verb == "GET") { this.GET__projects_statisticsHandler(); return; }
            }
            else if (StringIsAllDigits(parts[start + 1])){
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
        default: break;
      }
      throw new RouteNotMatchedException(verb, path);
    }
    
    static bool StringIsAllDigits(string s) {
      foreach (char c in s) {
        if (c < '0' || c > '9') { return false; }
      }
      return true;
    }
    
    public class RouteNotMatchedException : Exception {
      public string Verb { get; private set; }
      public string Path { get; private set; }
      public RouteNotMatchedException(string verb, string path) {
        this.Verb = verb;
        this.Path = path;
      }
    }
  }
}
```

