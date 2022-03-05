module Constants

open RuntimeTypes
open System.Collections.Generic

let EmptyEnvironment =  ImmutableEnvironment (new Dictionary<string, Ref<FLOXValue>>(0), None)

