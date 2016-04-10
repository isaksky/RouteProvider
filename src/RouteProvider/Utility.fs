namespace IsakSky.RouteProvider

open System
open System.IO

module Utility =
  type ObjectUtilities() =
    static let idStart = Random().Next(Int32.MaxValue) |> Convert.ToInt64
    static let idGen = new System.Runtime.Serialization.ObjectIDGenerator()

    static member GetInstanceId(o:obj) =
      let id, _ = idGen.GetId(o)
      id + idStart

  let loglock = obj()
  let inline log fmt =
    let cont (s:string) =
      let path = Path.Combine(__SOURCE_DIRECTORY__, "tp.log")
      lock loglock <| fun () ->
        use f = File.Open(path, FileMode.Append)
        use w = new StreamWriter(f)
        fprintfn w "%s" s
    Printf.kprintf cont fmt

  let getTmpFileName folder ext =
    let rng = new Random()
    let rec getFile attemptsLeft =
      let n = rng.Next(Int32.MaxValue)
      let filename = sprintf "%d.%s" n ext
      let path = Path.Combine(folder, filename)
      try
        File.Open(path, FileMode.CreateNew).Dispose()
        path
      with
      | :? IOException ->
        if attemptsLeft = 0 then reraise()
        else getFile (attemptsLeft - 1)

    getFile 5
