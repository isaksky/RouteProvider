module RouteProvider.Tests

open NUnit.Framework

[<Literal>]
let routesStr1 = """
  GET projects/{projectId}
  PUT projects/{projectId}/action
  POST people
  POST people/{personId}/status/{status:string}
"""

type Routes1 = IsakSky.RouteProvider<routesStr1>

[<Test>]
let ``2part route`` () =
  let routesHit = Array.create 4 false
  let router1 =
    Routes1(
      GET__projects = (fun p -> routesHit.[0] <- true),
      PUT__projects_action = (fun p -> routesHit.[1] <- true),
      POST__people = (fun () -> routesHit.[2] <- true),
      POST__people_status = (fun p s -> routesHit.[3] <- true)  
    )
  router1.DispatchRoute("GET", "/projects/1")
  Assert.IsTrue(routesHit.[0])
  Assert.IsFalse(routesHit.[1])
  Assert.IsFalse(routesHit.[2])
  Assert.IsFalse(routesHit.[3])

[<Test>]
let ``3part route`` () =
  let routesHit = Array.create 4 false
  let router1 =
    Routes1(
      GET__projects = (fun p -> routesHit.[0] <- true),
      PUT__projects_action = (fun p -> routesHit.[1] <- true),
      POST__people = (fun () -> routesHit.[2] <- true),
      POST__people_status = (fun p s -> routesHit.[3] <- true)  
    )
  router1.DispatchRoute("PUT", "/projects/1/action")
  Assert.IsFalse(routesHit.[0])
  Assert.IsTrue(routesHit.[1])
  Assert.IsFalse(routesHit.[2])
  Assert.IsFalse(routesHit.[3])

[<Test>]
let ``4part route`` () =
  let mutable routeHit = false
  let router1 =
    Routes1(
      GET__projects = (fun p -> ()),
      PUT__projects_action = (fun p -> ()),
      POST__people = (fun () -> ()),
      POST__people_status = (fun p s -> routeHit <- true)  
    )
  router1.DispatchRoute("POST", "people/123/status/chilling")
  Assert.IsTrue(routeHit)

[<Literal>]
let routesStr2 = """
  GET projects/statistics as getProjStats
  GET projects/{action:string} as getProjAction
"""

type Routes2 = IsakSky.RouteProvider<routesStr2>

[<Test>]
let ``const strings prioritized over dyn strings`` () =
  let mutable routeHit = false
  let router =
    Routes2(
      getProjStats = (fun () -> routeHit <- true),
      getProjAction = (fun s -> ())
    )
  router.DispatchRoute("GET", "projects/statistics")
  Assert.IsTrue(routeHit)

[<Test>]
let ``dyn strings captured`` () =
  let mutable action = ""
  let router =
    Routes2(
      getProjStats = (fun () -> ()),
      getProjAction = (fun routeAction -> action <- routeAction)
    )
  router.DispatchRoute("GET", "projects/aggregate")
  Assert.AreEqual(action, "aggregate")

type Routes3 = IsakSky.RouteProvider<routesStr2, "string">

[<Test>]
let ``input type passed in correctly`` () =
  let router =
    Routes3(
      getProjStats = (fun ctx -> Assert.AreEqual(ctx, "yep")),
      getProjAction = (fun ctx s -> ())
    )
  router.DispatchRoute("yep", "GET", "projects/statistics")

type Routes4 = IsakSky.RouteProvider<routesStr2, "", "string">

[<Test>]
let ``return type sent back`` () =
  let router =
    Routes4(
      getProjStats = (fun () -> ""),
      getProjAction = (fun routeAction -> routeAction)
    )
  let resp = router.DispatchRoute("GET", "projects/archive")
  Assert.AreEqual(resp, "archive")


[<Literal>]
let routesStr4 = """
  GET projects/statistics as getProjectStats
  GET projects/{projectId:int64} as getProject
  GET projects/{action:string} as getProjectAction
"""

type Routes5 = IsakSky.RouteProvider<routesStr4, "", "string">

[<Test>]
let ``conflicting routes ordering`` () =
  let router =
    Routes5(
      getProjectStats = (fun () -> "stats"),
      getProject = (fun projectId -> sprintf "%d" projectId),
      getProjectAction = (fun routeAction -> routeAction)
    )
  let resp = router.DispatchRoute("GET", "/projects/statistics")
  Assert.AreEqual(resp, "stats")
  
  let resp2 = router.DispatchRoute("GET", "/projects/archive")
  Assert.AreEqual(resp2, "archive")
  
  let resp3 = router.DispatchRoute("GET", "/projects/1")
  Assert.AreEqual(resp3, "1")