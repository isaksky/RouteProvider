(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r @"..\..\bin\RouteProvider\RouteProvider.dll"
(**
RouteProvider
======================
RouteProvider is an F# Type provider that generates types suitable for routing in a web application.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The RouteProvider library can be <a href="https://nuget.org/packages/RouteProvider">installed from NuGet</a>:
      <pre>PM> Install-Package RouteProvider</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------
This example initializes RouteProvider with 5 different routes:
*)

[<Literal>]
let routes = """
  GET projects/{projectId} as getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId:int} as updateProject
  GET projects/statistics
  GET people/{name:string} as getPerson
"""

type MyRoutes = IsakSky.RouteProvider<routes>

(**
We can then build routes using this type in a strongly typed way:
*)
let path1 = MyRoutes.Builders.getProjectComments(123L,4L)
let path2 = MyRoutes.Builders.getPerson("jack")
(**
We can also create a router:
*)
let router =
  MyRoutes(
    getProject = (fun projectId -> 
      printfn "You asked for project %d" projectId),
    getProjectComments = (fun projectId commentId ->
      printfn "You asked for project %d and comment %d" projectId commentId),
    updateProject = (fun p -> 
      printfn "Updated project %d" p),
    // If you don't provide a route name, one will be computed for you
    GET__projects_statistics = (fun () -> printfn "You asked for project statistics"),
    getPerson = (fun name -> printfn "You asked for a person called \"%s\"" name))

(**
We can then dispatch routes to this router like this, and the appropriate handler will be executed:
*)
router.DispatchRoute("GET", "projects/4321/comments/1234")
// "You asked for project 4321 and comment 1234"

(**
To integrate the router with existing projects, you can specify a fully qualified input and/or output type:

<img src="/RouteProvider/img/input_and_output_types.png">

As you can see above, the dispatch method and handler signatures will then be changed to accept and/or return these types.

Sample projects
-----------------------
 * [Simple](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/Simple/Simple/Simple.fsx) A basic command line example

 * [Owin Input type](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/SampleWithOwin/SampleWithOwin/Program.fs) A basic web app with a few routes, using an Owin input type

 * [ASP.Net MVC](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/MVC/Global.asax.fs) Taking over routing for ASP.NET MVC

 * [System Web](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/SystemWebReturnType/SystemWebReturnType/Program.fs) A basic web app, specifying a HttpListenerContext as an input type, and a string as a return type

 * [API Reference](reference/index.html) automatically generated documentation for all types

 Code generation samples:
 ------------------------

 * [Plain](notypes.html)

 * [Input type](input_type.html)

 * [Input and return type](input_and_return_type.html)
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/RouteProvider/tree/master/docs/content
  [gh]: https://github.com/fsprojects/RouteProvider
  [issues]: https://github.com/fsprojects/RouteProvider/issues
  [readme]: https://github.com/fsprojects/RouteProvider/blob/master/README.md
  [license]: https://github.com/fsprojects/RouteProvider/blob/master/LICENSE.txt
*)
