module Update

/// If the condition is true, return the transformed 
/// object, else return the object unchanged.
let inline UpdateIf condition transformed objekt =
    if condition then objekt |> transformed else objekt

/// If, when applied to the object, the condition is true, 
/// return the transformed object, else return the object unchanged.
let inline UpdateWhen condition transformed objekt =
    if objekt |> condition then objekt |> transformed else objekt

/// If the condition is true for the object, return the value, else
/// return the object unchanged.
let inline UpdateToValueWhen condition value objekt =
    if objekt |> condition then value else objekt
