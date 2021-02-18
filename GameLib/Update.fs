module Update

/// If the condition is true, return the transformed 
/// item, else return the item unchanged.
let inline UpdateIf condition transformed item =
    if condition then item |> transformed else item

/// If, when applied to the item, the condition is true, 
/// return the transformed item, else return the item unchanged.
let inline UpdateWhen condition transformed item =
    if item |> condition then item |> transformed else item

/// If the condition is true for the item, return the value, else
/// return the item unchanged.
let inline UpdateToValueWhen condition value item =
    if item |> condition then value else item

