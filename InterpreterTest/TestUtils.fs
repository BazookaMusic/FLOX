module Utilities

open Microsoft.VisualStudio.TestTools.UnitTesting

let rec AssertSequenceComparison (lista,listb, (comparison) , (fail: 'a list * 'a list -> unit) ) =
    match (lista,listb)  with
        | (ha::ta, hb::tb) -> 
            comparison (ha, hb)
            AssertSequenceComparison (ta, tb, comparison, fail)
        | ([],[]) -> ()
        | _ -> fail (lista,listb)