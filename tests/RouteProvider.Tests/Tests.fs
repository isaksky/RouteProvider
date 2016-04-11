module RouteProvider.Tests

open NUnit.Framework

[<Literal>]
let routesStr1 = """
  GET projects/{projectId}
  PUT projects/{projectId}/action
  POST people
  POST people/{personId}/status/{status:string}
"""
[<Literal>]
let outputPath1 = __SOURCE_DIRECTORY__ + "\MyRoutes.fs"

type Routes1 = IsakSky.RouteProvider<"MyRoutes",routesStr1, false, true, outputPath1, "Ns1">
open Ns1.MyModule

[<Test>]
let ``2part route`` () =
  let routesHit = Array.create 4 false
  let router1 =
    {
      GET__projects = (fun p -> routesHit.[0] <- true)
      PUT__projects_action = (fun p -> routesHit.[1] <- true)
      POST__people = (fun () -> routesHit.[2] <- true)
      POST__people_status = (fun p s -> routesHit.[3] <- true)
      notFound = None
    }
  router1.DispatchRoute("GET", "/projects/1")
  Assert.IsTrue(routesHit.[0])
  Assert.IsFalse(routesHit.[1])
  Assert.IsFalse(routesHit.[2])
  Assert.IsFalse(routesHit.[3])

[<Test>]
let ``3part route`` () =
  let routesHit = Array.create 4 false
  let router1 =
    {
      GET__projects = (fun p -> routesHit.[0] <- true)
      PUT__projects_action = (fun p -> routesHit.[1] <- true)
      POST__people = (fun () -> routesHit.[2] <- true)
      POST__people_status = (fun p s -> routesHit.[3] <- true)
      notFound = None
    }
  router1.DispatchRoute("PUT", "/projects/1/action")
  Assert.IsFalse(routesHit.[0])
  Assert.IsTrue(routesHit.[1])
  Assert.IsFalse(routesHit.[2])
  Assert.IsFalse(routesHit.[3])

[<Test>]
let ``4part route`` () =
  let mutable routeHit = false
  let router1 =
    {
      GET__projects = (fun p -> ())
      PUT__projects_action = (fun p -> ())
      POST__people = (fun () -> ())
      POST__people_status = (fun p s -> routeHit <- true)
      notFound = None
    }
  router1.DispatchRoute("POST", "people/123/status/chilling")
  Assert.IsTrue(routeHit)

[<Literal>]
let routesStr2 = """
  GET projects/statistics as getProjStats
  GET projects/{action:string} as getProjAction
"""

[<Literal>]
let outputPath2 = __SOURCE_DIRECTORY__ + "\MyRoutes2.fs"

type Routes2 = IsakSky.RouteProvider<"MyRoutes2",routesStr2, false, false, outputPath2, "Ns2">
open Ns2.MyModule

[<Test>]
let ``const strings prioritized over dyn strings`` () =
  let mutable routeHit = false
  let router =
    {
      MyRoutes2.getProjStats = (fun () -> routeHit <- true)
      getProjAction = (fun s -> ())
      notFound = None
    }
  router.DispatchRoute("GET", "projects/statistics")
  Assert.IsTrue(routeHit)

[<Test>]
let ``dyn strings captured`` () =
  let mutable action = ""
  let router =
    {
      MyRoutes2.getProjStats = (fun () -> ())
      getProjAction = (fun routeAction -> action <- routeAction)
      notFound = None
    }
  router.DispatchRoute("GET", "projects/aggregate")
  Assert.AreEqual(action, "aggregate")

[<Literal>]
let outputPath3 = __SOURCE_DIRECTORY__ + "\MyRoutes3.fs"
type Routes3 = IsakSky.RouteProvider<"MyRoutes3", routesStr2, true, true, outputPath3, "Ns3">
open Ns3.MyModule

[<Test>]
let ``input type passed in correctly`` () =
  let router =
    {
      MyRoutes3.getProjStats = (fun ctx -> Assert.AreEqual(ctx, "yep"))
      getProjAction = (fun ctx s -> ())
      notFound = None
    }
  router.DispatchRoute("yep", "GET", "projects/statistics")

[<Test>]
let ``return type sent back`` () =
  let router =
    {
      MyRoutes3.getProjStats = (fun _ -> "")
      getProjAction = (fun _ routeAction -> routeAction)
      notFound = None
    }
  let resp = router.DispatchRoute((), "GET", "projects/archive")
  Assert.AreEqual(resp, "archive")


[<Literal>]
let routesStr4 = """
  GET projects/statistics as getProjectStats
  GET projects/{projectId:int64} as getProject
  GET projects/{action:string} as getProjectAction
"""

let [<Literal>] outputPath4 = __SOURCE_DIRECTORY__ + "\MyRoutes4.fs"
type Routes4 = IsakSky.RouteProvider<"MyRoutes4", routesStr4, false, true, outputPath4, "Ns4">
open Ns4.MyModule

[<Test>]
let ``conflicting routes ordering`` () =
  let router =
    {
      MyRoutes4.getProjectStats = (fun () -> "stats")
      getProject = (fun projectId -> sprintf "%d" projectId)
      getProjectAction = (fun routeAction -> routeAction)
      notFound = None
    }
  let resp = router.DispatchRoute("GET", "/projects/statistics")
  Assert.AreEqual(resp, "stats")

  let resp2 = router.DispatchRoute("GET", "/projects/archive")
  Assert.AreEqual(resp2, "archive")

  let resp3 = router.DispatchRoute("GET", "/projects/1")
  Assert.AreEqual(resp3, "1")

[<Literal>]
let routesStr5 = """
  GET /projects/{projectId:int} as getProjectInt
  GET /projects/{projectId:int64} as getProjectInt64
  GET /projects/{projectId:Guid} as getProjectGuid
  GET /projects/{projectId:string} as getProjectString
"""
let [<Literal>] outputPath5 = __SOURCE_DIRECTORY__ + "\MyRoutes5.fs"
type Routes5 = IsakSky.RouteProvider<"MyRoutes5", routesStr5, false, true, outputPath5, "Ns5">
open Ns5.MyModule

[<Test>]
let ``guid routes work`` () =
  let router =
    {
      MyRoutes5.getProjectInt = fun _ -> None
      getProjectInt64 = fun _ -> None
      getProjectGuid = fun g -> Some(g)
      getProjectString = fun _ -> None
      notFound = None
    }
  let sampGuid = System.Guid.NewGuid()
  let path = Ns5.MyModule.getProjectGuid sampGuid
  let resp = router.DispatchRoute("GET", path)
  Assert.IsTrue(resp.IsSome)
  Assert.AreEqual(sampGuid, resp.Value)
