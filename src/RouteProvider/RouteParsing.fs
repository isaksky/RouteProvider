module RouteParsing

open Route
open FParsec

// Sample routes:
// GET projects/{project_id}
// POST projects/{project_id}
// DELETE projects/{project_id}

let pVerb = many1SatisfyL isAsciiUpper "1 OR more upper case letters (HTTP VERB)"
let pConstant = many1SatisfyL (fun c -> isLetter (c) || isDigit (c) || c = '_') "1 or more digit, letters or underscores"

let pConstantPathSeg = pConstant |>> Constant
let pIdPathSeg = between (pchar '{') (pchar '}') pConstant |>> NumericID
//let pConstantPathSeg = pConstant |>> fun s -> ConstantSeg(s) :> PathSegment
//let pIdPathSeg = between (pchar '{') (pchar '}') pConstant |>> fun s -> Int64Seg(s) :> PathSegment

let pPathSeg = choice [ pConstantPathSeg; pIdPathSeg ]
let pPath = sepEndBy1 pPathSeg (pchar '/')
let pRoute = pVerb .>>. (spaces >>. pPath) |>> fun (verb, segs) -> { verb=verb; routeSegments = segs }
//let pRoute = pVerb .>>. (spaces >>. pPath) |>> fun (verb, segs) -> 
//                                                let segs2 = new ResizeArray<PathSegment>()
//                                                for seg in segs do segs2.Add(seg)
//                                                Route(verb, segs2)
let pRoutes : Parser<_, unit> = spaces >>. sepEndBy1 pRoute spaces .>> eof
