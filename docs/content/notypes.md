Configuration:

    type MyRoutes = IsakSky.RouteProvider<routes>


Generated code:

    [lang=csharp]
    using System;
    namespace IsakSky {
      public class MyRoutes {
        public static class Builders{
          public static string getProject(long projectId){
            return string.Format("/projects/{0}", projectId);
          }
          public static string getProjectComments(long projectId, long commentId){
            return string.Format("/projects/{0}/comments/{1}", projectId, commentId);
          }
          public static string updateProject(int projectId){
            return string.Format("/projects/{0}", projectId);
          }
          public static string GET__projects_statistics(){
            return "/projects/statistics";
          }
          public static string getPerson(string name){
            return string.Format("/people/{0}", name);
          }
        }
        public MyRoutes(
          Action<long> getProject,
          Action<long, long> getProjectComments,
          Action<int> updateProject,
          Action GET__projects_statistics,
          Action<string> getPerson,
          Action<string, string> notFound = null) {
            this.getProject = getProject;
            this.getProjectComments = getProjectComments;
            this.updateProject = updateProject;
            this.GET__projects_statistics = GET__projects_statistics;
            this.getPerson = getPerson;
            this.notFound = notFound;
          }
        public readonly Action<long> getProject;
        public readonly Action<long, long> getProjectComments;
        public readonly Action<int> updateProject;
        public readonly Action GET__projects_statistics;
        public readonly Action<string> getPerson;
        public readonly Action<string, string> notFound;
    
        public void DispatchRoute(string verb, string path) {
          var parts = path.Split('/');
          var start = 0;
          if (parts[0] == "") { start = 1; }
          var endOffset = parts.Length > 0 && parts[parts.Length - 1] == "" ? 1 : 0;
          switch (parts.Length - start - endOffset) {
            case 2:
              if (parts[start + 0] == "people"){
                {
                  var name = parts[start + 1];
                  if (verb == "GET") { this.getPerson(name); return; }
                }
              }
              if (parts[start + 0] == "projects"){
                if (parts[start + 1] == "statistics"){
                  if (verb == "GET") { this.GET__projects_statistics(); return; }
                }
                else if (StringIsAllDigits(parts[start + 1])){
                  var projectId = long.Parse(parts[start + 1]);
                  if (verb == "GET") { this.getProject(projectId); return; }
                }
                else if (StringIsAllDigits(parts[start + 1])){
                  var projectId = int.Parse(parts[start + 1]);
                  if (verb == "PUT") { this.updateProject(projectId); return; }
                }
              }
              break;
            case 4:
              if (parts[start + 0] == "projects"){
                if (StringIsAllDigits(parts[start + 1])){
                  var projectId = long.Parse(parts[start + 1]);
                  if (parts[start + 2] == "comments"){
                    if (StringIsAllDigits(parts[start + 3])){
                      var commentId = long.Parse(parts[start + 3]);
                      if (verb == "GET") { this.getProjectComments(projectId, commentId); return; }
                    }
                  }
                }
              }
              break;
            default: break;
          }
          if (this.notFound == null) { throw new RouteNotMatchedException(verb, path); }
          else { this.notFound(verb, path); }
        }
        static bool StringIsAllDigits(string s) {
          foreach (char c in s) {
            if (c < '0' || c > '9') { return false; }
          }
          return true;
        }
        public class RouteNotMatchedException : Exception {
          public string Verb { get; private set; }
          public string Path { get; private set; }
          public RouteNotMatchedException(string verb, string path) {
            this.Verb = verb;
            this.Path = path;
          }
        }
      }
    }
    
    