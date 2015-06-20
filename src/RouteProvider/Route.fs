module Route

type NamedRouteSegment = 
  | Constant of string
  | NumericID of string

type Route =
    { routeSegments: NamedRouteSegment list
      verb: string}

//type Route(verb: string, routeSegments: NamedRouteSegment list) =
//  member x.verb = verb
//  member x.routeSegments = routeSegments

