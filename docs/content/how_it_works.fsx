#r @"..\..\bin\RouteProvider\RouteProvider.dll"


(*** hide ***)

[<Literal>]
let routes = """
  GET projects/{projectId} as getProject
  GET projects/{projectId}/comments/{commentId} as getProjectComments
  PUT projects/{projectId:int} as updateProject
  GET projects/statistics
  GET people/{name:string} as getPerson
"""

