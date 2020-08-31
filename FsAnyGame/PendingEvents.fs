/// Experimental (simpler!) future events handling.
module PendingEvents

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
type FutureApplicationResult<'state,'changeData> =
    | FutureDone of 'state
    | FutureDoneWithAdditionalToDos of 'state * Pending<'changeData> list



/// Fold to-do list, calling action handler for those ToDoItems that are due.
/// The user's action handler can make use of an accumulator of a user-defined type.
/// Returns the user's resulting accumulator, and the list of the remaining items that were not done.
let AppliedForTime 
    gameTime 
    (f:'state -> 'changeData -> float32<seconds> -> FutureApplicationResult<'state,'changeData>) 
    (struct ((state:'state) , (pendingList:Pending<'changeData> list))) 
        : (struct ('state * Pending<'changeData> list)) =

    // TODO: Apply in the order of oldest-first.

    // Performance optimisation if none due (could be very high percentage of cases):

    if pendingList
        |> List.exists (fun { ToDoTime=itemTime } -> gameTime >= itemTime) then

        // Something is due, so process:

        pendingList 
            |> List.fold (fun struct (actionAccumulator, newListAccumulator) toDoItem ->
        
                let { ToDoTime=itemTime ; ToDoData=data } = toDoItem

                if gameTime >= itemTime then

                    // This item is due, so call the action handler and obtain
                    // a new user-accumulator.  We drop the toDoItem.

                    match f actionAccumulator data gameTime with

                        | FutureDone newState -> 
                            struct (newState , newListAccumulator)

                        | FutureDoneWithAdditionalToDos (newState , additionals) -> 
                            struct (newState , (List.append additionals newListAccumulator))

                else

                    // This item is NOT due, so re-attach to the return to-do list:

                    struct (actionAccumulator, toDoItem::newListAccumulator)

            ) struct (state, [])

    else
        struct (state, pendingList)

