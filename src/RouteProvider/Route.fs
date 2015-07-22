module Route

type PathSegment (name:string) = 
  member x.name = name
type ConstantSeg (name:string) =
  inherit PathSegment(name)
type Int64Seg (name:string) =
  inherit PathSegment(name)

type NamedRouteSegment = 
 | Constant of string
 | NumericID of string

type Route(verb: string, routeSegments: ResizeArray<PathSegment>) =
  member x.verb = verb
  member x.routeSegments = routeSegments

type Route2 =
   { routeSegments: NamedRouteSegment list
     verb: string}