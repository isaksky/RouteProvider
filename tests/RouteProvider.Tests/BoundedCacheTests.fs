module RouteProvider.BoundedCacheTests

open NUnit.Framework
open IsakSky.RouteProvider

[<Test>]
let ``old items get evicted`` () =
  let bcache = BoundedCache<_,_>(3)

  bcache.Add("one", "First")  
  bcache.Add("two", "second")
  bcache.Add("three", "third")
  Assert.AreEqual("First", bcache.Get("one"))
  bcache.Add("four", "fourth")
  Assert.AreEqual(bcache.TryGet("one"), None)

[<Test>]
let ``old keys that dont point to their original value dont get evicted`` () =
  let bcache = BoundedCache<_,_>(3)
  bcache.Add("one", "First") 
  bcache.Add("two", "second")
  bcache.Add("one", "New")  
  bcache.Add("four", "fourth")
  Assert.AreEqual(bcache.Get("one"), "New")

