module Algorithm

open Random

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
let ChooseItemFromListBasedOnGameTime (theList:'a list) (gameTime:GameTime) =
    let n = theList.Length
    if n = 0 then
        failwith "Cannot choose one thing from an empty list"
    else if n = 1 then
        theList.[0]
    else
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


/// Like List.map, except the mapping function is also passed the remainder of the list
/// to be done, and the new list as it stands so far in the process.
let UpgradedListMap (mappingFunction:'item -> 'item list -> 'output list -> 'output) theList = // TODO: rename

    let mutable accLeftToDo  = theList
    let mutable doneList = []

    while not (accLeftToDo |> List.isEmpty) do
        match accLeftToDo with
            | item::toDoList -> 
                let processedItem = mappingFunction item toDoList doneList
                doneList <- processedItem :: doneList
                accLeftToDo <- toDoList
            | [] ->
                ()

    doneList |> List.rev



/// Returns a sequence that yields the contents of the given array in a shuffled order.
let ShuffledArrayAsSeq (things:'t[]) randomSeed =

    let mutable randomState = randomSeed
    let count = things.Length

    /// Temporary indirection array to avoid modifying caller's array.
    let indirection = Array.init count (fun i -> i)

    let sequence =
        seq {
            for i = (count-1) downto 0 do
                randomState <- randomState |> XorShift32
                let (XorShift32State v) = randomState

                // Choose from the remaining indices:
                let index = (int) (v % (uint32) (i+1))

                yield things.[indirection.[index]]

                // Swap chosen index to end:
                let v = indirection.[index]
                indirection.[index] <- indirection.[i]
                indirection.[i] <- v
        }

    (sequence, randomState)


