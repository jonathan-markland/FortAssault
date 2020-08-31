module ScoreboardTests

open Xunit
open FsXunit
open ScoreboardModel



let StartingScoreboard = 
    InitialScoreboard [ "Fred" ; "Wilma" ; "Barney" ; "Betty" ] 5000u 1000u




[<Fact>]
let ``We can make a scoreboard`` () =

    StartingScoreboard
        |> ShouldEqual 
            [
                { PlayerName = "Betty"  ; PlayerScore = 8000u ; PlayerScoreString = "8000" }
                { PlayerName = "Barney" ; PlayerScore = 7000u ; PlayerScoreString = "7000" }
                { PlayerName = "Wilma"  ; PlayerScore = 6000u ; PlayerScoreString = "6000" }
                { PlayerName = "Fred"   ; PlayerScore = 5000u ; PlayerScoreString = "5000" }
            ]



[<Fact>]
let ``The player beats the lowest score`` () =

    let newScore =
        { NewPlayerName = "Jonathan" ; NewPlayerScore = 5500u }

    let scoreboard' =
        StartingScoreboard |> WithNewScoreIncluded newScore

    scoreboard'
        |> ShouldEqual 
            [
                { PlayerName = "Betty"    ; PlayerScore = 8000u ; PlayerScoreString = "8000" }
                { PlayerName = "Barney"   ; PlayerScore = 7000u ; PlayerScoreString = "7000" }
                { PlayerName = "Wilma"    ; PlayerScore = 6000u ; PlayerScoreString = "6000" }
                { PlayerName = "Jonathan" ; PlayerScore = 5500u ; PlayerScoreString = "5500" }
            ]




[<Fact>]
let ``The player beats the highest score`` () =

    let newScore =
        { NewPlayerName = "Jonathan" ; NewPlayerScore = 15500u }

    let scoreboard' =
        StartingScoreboard |> WithNewScoreIncluded newScore

    scoreboard'
        |> ShouldEqual 
            [
                { PlayerName = "Jonathan" ; PlayerScore = 15500u ; PlayerScoreString = "15500" }
                { PlayerName = "Betty"    ; PlayerScore = 8000u ; PlayerScoreString = "8000" }
                { PlayerName = "Barney"   ; PlayerScore = 7000u ; PlayerScoreString = "7000" }
                { PlayerName = "Wilma"    ; PlayerScore = 6000u ; PlayerScoreString = "6000" }
            ]
    



[<Fact>]
let ``Score lower than the lowest cannot enter board`` () =
    1000u
        |> ScoreCanEnterBoard StartingScoreboard
        |> ShouldBeFalseWhenDoing "can store enter board"


[<Fact>]
let ``Score equalling lowest can enter board`` () =
    5000u
        |> ScoreCanEnterBoard StartingScoreboard
        |> ShouldBeTrueWhenDoing "can store enter board"


[<Fact>]
let ``Score one point below lowest cannot enter board`` () =
    4999u
        |> ScoreCanEnterBoard StartingScoreboard
        |> ShouldBeFalseWhenDoing "can store enter board"


[<Fact>]
let ``Score higher than the highest can enter board`` () =
    10000u
        |> ScoreCanEnterBoard StartingScoreboard
        |> ShouldBeTrueWhenDoing "can store enter board"


[<Fact>]
let ``Score mid-board can enter board`` () =
    6500u
        |> ScoreCanEnterBoard StartingScoreboard
        |> ShouldBeTrueWhenDoing "can store enter board"



[<Fact>]
let ``Score board text`` () =

    let text =
        StartingScoreboard
            |> ScoreboardText 30

    text
        |> ShouldEqual
            [
                "Betty ................... 8000"
                "Barney .................. 7000"
                "Wilma ................... 6000"
                "Fred .................... 5000"
            ]

    text
        |> List.forall (fun s -> s.Length = 30)
        |> ShouldBeTrueWhenDoing "checking the lengths of the scoreboard strings"

