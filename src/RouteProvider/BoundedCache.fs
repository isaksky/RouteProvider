namespace IsakSky.RouteProvider

open System.Collections.Generic
open System.Threading

type BoundedCache<'a, 'b when 'a : equality and 'b : equality>(max: int) =
  let dict = new Dictionary<'a, 'b>()
  let queue = new Queue<'a * 'b>(max)
  let locker = new ReaderWriterLockSlim()

  member this.Add(key:'a, value:'b) =
    locker.EnterWriteLock()
    if queue.Count = max then
      let (k, v) = queue.Dequeue()
      if dict.[k] = v then dict.Remove(k) |> ignore
    dict.[key] <- value
    queue.Enqueue((key, value))
    locker.ExitWriteLock()

  member this.Get(key: 'a) =
    locker.EnterReadLock()
    let v = dict.[key]
    locker.ExitReadLock()
    v

  member this.TryGet(key: 'a) =
    locker.EnterReadLock()
    let v = if dict.ContainsKey(key) then Some(dict.[key]) else None
    locker.ExitReadLock()
    v
