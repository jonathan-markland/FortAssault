/// Move items from one input list to two output lists, or drop items.
/// The types can be transformed as the items are transferred.
module ListSplicer



type ListItemSelectAction<'t1,'t2> = 
    | FirstList of 't1
    | SecondList of 't2
    | DropListItem



/// Choose items from a primary list and direct them onto two
/// lists that are returned.  There is opportunity to change the
/// types as the items are placed onto the new lists, and also
/// to drop input list items from the final result.  The result
/// lists will be in the relative order of the reversed input list.
let ReverseListSplicedBy selectorFunction (lst:'item list) =

    let mutable firstList  = []
    let mutable secondList = []
    let mutable pos        = lst

    while not pos.IsEmpty do
        match pos with
            | [] -> ()
            | head::tail ->
                let actionDesired = selectorFunction head
                match actionDesired with
                    | FirstList (newItem:'t1)  -> firstList  <- newItem::firstList
                    | SecondList (newItem:'t2) -> secondList <- newItem::secondList
                    | DropListItem -> ()
                pos <- tail

    struct (firstList, secondList)



/// Choose items from a primary list and direct them onto two
/// lists that are returned.  There is opportunity to change the
/// types as the items are placed onto the new lists, and also
/// to drop input list items from the final result.  The result
/// lists will retain the relative order of the input list.
let ListSplicedBy selectorFunction (lst:'item list) =

    let struct (firstList, secondList) = 
        lst |> ReverseListSplicedBy selectorFunction

    struct (firstList |> List.rev , secondList |> List.rev)



let SplicedListOfResults listOfResults =
    
    let whetherItsGoodOrError item =
        match item with
            | Ok x -> FirstList x
            | Error x -> SecondList x

    listOfResults |> ListSplicedBy whetherItsGoodOrError

