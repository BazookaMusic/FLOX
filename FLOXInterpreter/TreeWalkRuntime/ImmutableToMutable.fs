module ImmutableToMutable

let ToList (a: 'a list) : ResizeArray<'a> =
    ResizeArray<'a> a
    

