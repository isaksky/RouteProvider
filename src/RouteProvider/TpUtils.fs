module TpUtils

open System

type internal UriResolutionType = 
  | DesignTime
  | Runtime
  | RuntimeInFSI

type internal UriResolver = 
  { ResolutionType : UriResolutionType
    DefaultResolutionFolder : string
    ResolutionFolder : string }
  
  static member Create(resolutionType, defaultResolutionFolder, resolutionFolder) = 
    { ResolutionType = resolutionType
      DefaultResolutionFolder = defaultResolutionFolder
      ResolutionFolder = resolutionFolder }
  
  /// Resolve the absolute location of a file (or web URL) according to the rules
  /// used by standard F# type providers as described here:
  /// https://github.com/fsharp/fsharpx/issues/195#issuecomment-12141785
  ///
  ///  * if it is web resource, just return it
  ///  * if it is full path, just return it
  ///  * otherwise.
  ///
  ///    At design-time:
  ///      * if the user specified resolution folder, use that
  ///      * otherwise use the default resolution folder
  ///    At run-time:
  ///      * if the user specified resolution folder, use that
  ///      * if it is running in F# interactive (config.IsHostedExecution) 
  ///        use the default resolution folder
  ///      * otherwise, use 'CurrentDomain.BaseDirectory'
  /// returns an absolute uri * isWeb flag
  member x.Resolve(uri : Uri) = 
    let root = 
      match x.ResolutionType with
      | DesignTime -> 
        if String.IsNullOrEmpty x.ResolutionFolder then x.DefaultResolutionFolder
        else x.ResolutionFolder
      | RuntimeInFSI -> x.DefaultResolutionFolder
      | Runtime -> AppDomain.CurrentDomain.BaseDirectory.TrimEnd('\\', '/')
    Uri(System.IO.Path.Combine(root, uri.OriginalString), UriKind.Absolute)