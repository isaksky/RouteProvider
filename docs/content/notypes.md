Configuration:

    type MyRoutes = IsakSky.RouteProvider<routes>


Generated code:

    [lang=fsharp]
    namespace IsakSky
    open System
    module RouteProvider =
      let getProject (projectId:int64) =
          "projects/" + projectId.ToString()
      let getProjectComments (projectId:int64) (commentId:int64) =
          "projects/" + projectId.ToString() + "comments/" + commentId.ToString()
      let updateProject (projectId:int) =
          "projects/" + projectId.ToString()
      let GET__projects_statistics  =
          "projects/statistics/"
      let getPerson (name:string) =
          "people/" + name
    
      module Internal =
        let stringIsAllDigits (s:string) =
          let mutable i = 0
          let mutable foundNonDigit = false
          while i < s.Length && not foundNonDigit do
            let c = s.[i]
            if c < '0' || c > '9' then foundNonDigit <- true
            i <- i + 1
          not foundNonDigit
        exception RouteNotMatchedException of string * string
    
      type MyRoutes =
        { getProject: int64->unit
          getProjectComments: int64->int64->unit
          updateProject: int->unit
          GET__projects_statistics: unit->unit
          getPerson: string->unit
          notFound: (string->string->unit) option }
    
        member this.DispatchRoute (verb:string) (path:string) : unit =
          let parts = path.Split('/')
          let start = if parts.[0] = "" then 1 else 0
          let endOffset = if parts.Length > 0 && parts.[parts.Length - 1] = "" then 1 else 0
          match parts.Length - start - endOffset with
          | 2 ->
            if parts.[start + 0] = "people" then (* hi *)
              let name = parts.[start + 1]
              if verb = "GET" then (* ho *) 
                this.getPerson name
              else
                match this.notFound with
                | None -> raise (Internal.RouteNotMatchedException (verb, path))
                | Some(notFound) -> notFound verb path
    
            elif parts.[start + 0] = "projects" then (* hi *)
              if parts.[start + 1] = "statistics" then (* hi *)
                if verb = "GET" then (* ho *) 
                  this.GET__projects_statistics 
                else
                  match this.notFound with
                  | None -> raise (Internal.RouteNotMatchedException (verb, path))
                  | Some(notFound) -> notFound verb path
    
              elif Internal.stringIsAllDigits(parts.[start + 1]) then (* hi *)
                let projectId = Int32.Parse(parts.[start + 1])
                if verb = "PUT" then (* ho *) 
                  this.updateProject projectId
                else
                  match this.notFound with
                  | None -> raise (Internal.RouteNotMatchedException (verb, path))
                  | Some(notFound) -> notFound verb path
    
              elif Internal.stringIsAllDigits(parts.[start + 1]) then (* hi *)
                let projectId = Int64.Parse(parts.[start + 1])
                if verb = "GET" then (* ho *) 
                  this.getProject projectId
                else
                  match this.notFound with
                  | None -> raise (Internal.RouteNotMatchedException (verb, path))
                  | Some(notFound) -> notFound verb path
    
              else
                match this.notFound with
                | None -> raise (Internal.RouteNotMatchedException (verb, path))
                | Some(notFound) -> notFound verb path
    
            else
              match this.notFound with
              | None -> raise (Internal.RouteNotMatchedException (verb, path))
              | Some(notFound) -> notFound verb path
          | 4 ->
            if parts.[start + 0] = "projects" then (* ho *) 
              if Internal.stringIsAllDigits(parts.[start + 1]) then (* ho *) 
                let projectId = Int64.Parse(parts.[start + 1])
                if parts.[start + 2] = "comments" then (* ho *) 
                  if Internal.stringIsAllDigits(parts.[start + 3]) then (* ho *) 
                    let commentId = Int64.Parse(parts.[start + 3])
                    if verb = "GET" then (* ho *) 
                      this.getProjectComments projectId commentId
                    else
                      match this.notFound with
                      | None -> raise (Internal.RouteNotMatchedException (verb, path))
                      | Some(notFound) -> notFound verb path
    
                  else
                    match this.notFound with
                    | None -> raise (Internal.RouteNotMatchedException (verb, path))
                    | Some(notFound) -> notFound verb path
    
                else
                  match this.notFound with
                  | None -> raise (Internal.RouteNotMatchedException (verb, path))
                  | Some(notFound) -> notFound verb path
    
              else
                match this.notFound with
                | None -> raise (Internal.RouteNotMatchedException (verb, path))
                | Some(notFound) -> notFound verb path
    
            else
              match this.notFound with
              | None -> raise (Internal.RouteNotMatchedException (verb, path))
              | Some(notFound) -> notFound verb path
          | _ ->
            match this.notFound with
            | None -> raise (Internal.RouteNotMatchedException (verb, path))
            | Some(notFound) -> notFound verb path
    
    