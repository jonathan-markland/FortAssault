/// Experimental (simpler!) future events handling.
module PendingEvents // TODO: I want to get rid of this.

open Time

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// A "To Do" item that should be considered at a given future time.
type Pending<'changeData> =
    {
        /// The future time at which to apply this.
        ToDoTime: float32<seconds>

        /// Some data to be quoted when applying the change.
        ToDoData: 'changeData
    }



let ToBeDoneAtTime t data =
    {
        ToDoTime = t
        ToDoData = data
    }



[<Struct>]
type PendingApplicationResult<'state,'changeData> =
    | PendingDone of 'state
    | PendingDoneWithAdditionalToDos of 'state * Pending<'changeData> list



let ReverseSecond (struct (a,b)) =  struct (a, b |> List.rev)



/// Fold to-do list, calling action handler for those ToDoItems that are due.
/// The user's action handler can make use of an accumulator of a user-defined type.
/// Returns the user's resulting accumulator, and the list of the remaining items that were not done.
let AppliedForTime 
    gameTime 
    (f:'state -> 'changeData -> float32<seconds> -> PendingApplicationResult<'state,'changeData>) 
    (struct ((state:'state) , (pendingList:Pending<'changeData> list))) 
        : (struct ('state * Pending<'changeData> list)) =

    // Garbage and performance optimisation if no items are due (could be very high percentage of cases):

    let isDueAt gameTime { ToDoTime=itemTime } = gameTime >= itemTime

    if pendingList |> List.exists (isDueAt gameTime) then

        // Something is due, so process:

        pendingList 

            |> List.rev // Consider items in the order of the first that was added.

            |> List.fold (fun struct (actionAccumulator, newListAccumulator) toDoItem ->
        
                if toDoItem |> isDueAt gameTime then

                    // This item is due, so call the action handler and obtain
                    // a new user-accumulator.  We drop the toDoItem.

                    match f actionAccumulator toDoItem.ToDoData gameTime with

                        | PendingDone newState -> 
                            struct (newState , newListAccumulator)

                        | PendingDoneWithAdditionalToDos (newState , additionals) -> 
                            struct (newState , (List.append additionals newListAccumulator))

                else

                    // This item is NOT due, so let's keep the toDoItem:

                    struct (actionAccumulator, toDoItem::newListAccumulator)

            ) struct (state, [])

            |> ReverseSecond // because the fold will have effectively reversed the list.

    else
        struct (state, pendingList)

