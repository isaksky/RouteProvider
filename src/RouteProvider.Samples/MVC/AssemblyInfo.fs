namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("RouteProviderMVC")>]
[<assembly: AssemblyProductAttribute("RouteProvider")>]
[<assembly: AssemblyDescriptionAttribute("A type provider for web routing")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
