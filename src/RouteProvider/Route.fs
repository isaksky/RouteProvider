module Route

type NamedRouteSegment = 
 | ConstantSeg of string
 | Int64Seg of string
 | StringSeg of string
 | IntSeg of string

type Route =
   { routeSegments: NamedRouteSegment list
     verb: string
     routeName: string option }