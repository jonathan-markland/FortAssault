module ListSplicerTests

open Xunit
open FsXunit
open ListSplicer



let MockData = [ 1 ; 2 ; 3 ; 4 ; 5 ]



[<Fact>]
let ``Splice all to the first list`` () =

    let struct (l,r) = MockData |> ListSplicedBy FirstList
    l |> ShouldEqual [ 1 ; 2 ; 3 ; 4 ; 5 ]
    r |> ShouldEqual []


[<Fact>]
let ``Splice all to the second list`` () =

    let struct (l,r) = MockData |> ListSplicedBy SecondList
    l |> ShouldEqual []
    r |> ShouldEqual [ 1 ; 2 ; 3 ; 4 ; 5 ]


[<Fact>]
let ``Splice all to the dustbin`` () =

    let struct (l,r) = MockData |> ListSplicedBy (fun _ -> DropListItem)
    l |> ShouldEqual []
    r |> ShouldEqual []


[<Fact>]
let ``Splice evens to first odds to second`` () =

    let struct (l,r) = MockData |> ListSplicedBy (fun x -> if x &&& 1 = 0 then FirstList(x) else SecondList(x))
    l |> ShouldEqual [ 2 ; 4 ]
    r |> ShouldEqual [ 1 ; 3 ; 5 ]


[<Fact>]
let ``Splice doubled evens to first and tripled odds to second`` () =

    let struct (l,r) = MockData |> ListSplicedBy (fun x -> if x &&& 1 = 0 then FirstList(x * 2) else SecondList(x * 3))
    l |> ShouldEqual [ 4 ; 8 ]
    r |> ShouldEqual [ 3 ; 9 ; 15 ]


[<Fact>]
let ``Splice doubled evens to first and tripled odds to second and 5 to the bin`` () =

    let struct (l,r) = 
        MockData |> ListSplicedBy 
            (fun x -> 
                if x = 5 then DropListItem else
                    (if x &&& 1 = 0 then FirstList(x * 2) else SecondList(x * 3)))

    l |> ShouldEqual [ 4 ; 8 ]
    r |> ShouldEqual [ 3 ; 9 ]


