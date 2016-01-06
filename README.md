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

It generates CSharp types. For example, for the routes defined above, it generates the following CSharp:

```CSharp
using System;
namespace IsakSky {
  public class MyRoutes {
    public static class Builders{
      public static string getProject(long projectId){
        return string.Format("/projects/{0}", projectId);
      }
      public static string getProjectComments(long projectId, long commentId){
        return string.Format("/projects/{0}/comments/{1}", projectId, commentId);
      }
      public static string updateProject(int projectId){
        return string.Format("/projects/{0}", projectId);
      }
      public static string GET__projects_statistics(){
        return "/projects/statistics";
      }
      public static string getPerson(string name){
        return string.Format("/people/{0}", name);
      }
    }
    public MyRoutes(
      Action<long> getProject,
      Action<long, long> getProjectComments,
      Action<int> updateProject,
      Action GET__projects_statistics,
      Action<string> getPerson) {
        this.getProject = getProject;
        this.getProjectComments = getProjectComments;
        this.updateProject = updateProject;
        this.GET__projects_statistics = GET__projects_statistics;
        this.getPerson = getPerson;
      }
    public readonly Action<long> getProject;
    public readonly Action<long, long> getProjectComments;
    public readonly Action<int> updateProject;
    public readonly Action GET__projects_statistics;
    public readonly Action<string> getPerson;

    public void DispatchRoute(string verb, string path) {
      var parts = path.Split('/');
      var start = 0;
      if (parts[0] == "") { start = 1; }
      var endOffset = parts.Length > 0 && parts[parts.Length - 1] == "" ? 1 : 0;
      switch (parts.Length - start - endOffset) {
        case 2:
          if (parts[start + 0] == "people"){
            {
              var name = parts[start + 1];
              if (verb == "GET") { this.getPerson(name); return; }
            }
          }
          if (parts[start + 0] == "projects"){
            if (parts[start + 1] == "statistics"){
              if (verb == "GET") { this.GET__projects_statistics(); return; }
            }
            else if (StringIsAllDigits(parts[start + 1])){
              var projectId = long.Parse(parts[start + 1]);
              if (verb == "GET") { this.getProject(projectId); return; }
            }
            else if (StringIsAllDigits(parts[start + 1])){
              var projectId = int.Parse(parts[start + 1]);
              if (verb == "PUT") { this.updateProject(projectId); return; }
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
                  if (verb == "GET") { this.getProjectComments(projectId, commentId); return; }
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

