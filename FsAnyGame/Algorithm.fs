module Algorithm

/// Returns true if 'item' does not exist in 'theList', where keySelector is
/// a function that returns a comparison key, given the item.
let NotInListById theList keySelector item =
    
    let keyOfItemToFind = item |> keySelector
    
    theList 
        |> List.tryFind (fun listItem -> 
            let thisItemKey = listItem |> keySelector
            keyOfItemToFind = thisItemKey) 

        |> Option.isNone


/// If all items in the list satisfy the predicate then
/// just return the original list.  Otherwise, perform
/// a List.filter to reduce the list to only those that
/// satisfy the predicate.
let PlanetSavingListFilter predicate theList =

    if theList |> List.forall predicate then
        theList
    else
        theList |> List.filter predicate



/// Returns alternate of two items depending on least 
/// significant bit of integer value.
let AlternateOf a b (value:int) =
    if value &&& 1 = 0 then a else b



open Time

/// Choose an item from the list based on elapsed time.
/// The time must be convertible to int, and is used as an index with (%).
/// This function throws an exception if theList is empty.
let ChooseItemFromListBasedOnGameTime theList (gameTime:float32<seconds>) =
    match theList with
        | [] -> failwith "Cannot choose one thing from an empty list"
        | _  -> 
            let n = theList.Length - 1
            let i = gameTime |> int |> abs
            let index = i % n
            theList.[index]


/// Only intended for use with very short lists, and where
/// creating a temporary aliasing array would be undesireable.
let inline ChooseItemFromListByModulo (hint:^num) (theList: 'a list) =
    
    let len = theList.Length
    
    if len > 0 then
        let idx = (int hint) % len
        Some(theList.[idx])

    else    
        None


