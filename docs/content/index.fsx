(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"
#r @"..\..\src\RouteProvider\bin\Debug\RouteProvider.dll"
#load "MyRoutes.fs"
(**
RouteProvider
======================
RouteProvider is an F# Type provider that generates types suitable for routing in a web application.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The RouteProvider library can be <a href="https://nuget.org/packages/RouteProvider">installed from NuGet</a>:
      <pre>PM> Install-Package RouteProvider -Pre</pre>
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
  PUT projects/{foo:string} as updateProject
  POST projects/{projectName:string} as createProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
"""

[<Literal>]
let outputPath = __SOURCE_DIRECTORY__ + "\MyRoutes.fs"

type Dummy = IsakSky.RouteProvider<"MyRoutes", // name of generated type
    routes,     // string of routes to base routes on
    false,      // add a generic input type?
    false,      // add a generic output type?
    outputPath>

(** From this invocation, we'll get a type generated that looks like this *)
open MyNamespace
open MyNamespace.MyModule // default namespace and module names
type HoverMe = MyRoutes // Hover the type to see the signature

(**
We can then build routes using this type in a strongly typed way:
*)
let path1 = MyModule.getProjectComments 123L 4L
let path2 = MyModule.createProject "Voltron"
(**
We can also create a router:
*)
let router : MyRoutes =
  { getProject = fun p -> printfn "Hi project %d" p
    updateProject = fun ps -> printfn "Hi project string %s" ps
    getProjectComments = fun p c -> printfn "Hi project comment %d %d" p c
    createProject = fun p -> printfn "Creating project %d" p
    notFound = None }

(**
We can then dispatch routes to this router like this, and the appropriate handler will be executed:
*)
router.DispatchRoute("GET", "projects/4321/comments/1234")
// "You asked for project 4321 and comment 1234"

(**
RouteProvider can also provide generic input arguments to integrate with web libraries. See the Suave example.

Sample projects
-----------------------
 * [Simple](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/Simple/Simple/Simple.fsx) A basic command line example

 * [Owin Input type](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/SampleWithOwin/SampleWithOwin/Program.fs) A basic web app with a few routes, using an Owin input type

 * [ASP.Net MVC](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/MVC/Global.asax.fs) Taking over routing for ASP.NET MVC

 * [Suave](https://github.com/isaksky/RouteProvider/blob/master/src/RouteProvider.Samples/WithSuave/WithSuave/WithSuave/Program.fs) An example with Suave, integrating by using a generic input and return type.

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

The library is available under the MIT license. For more information see the
[License file][license] in the GitHub repository.

  [content]: https://github.com/isaksky/RouteProvider/tree/master/docs/content
  [gh]: https://github.com/isaksky/RouteProvider
  [issues]: https://github.com/isaksky/RouteProvider/issues
  [readme]: https://github.com/isaksky/RouteProvider/blob/master/README.md
  [license]: https://github.com/isaksky/RouteProvider/blob/master/LICENSE.txt
*)
