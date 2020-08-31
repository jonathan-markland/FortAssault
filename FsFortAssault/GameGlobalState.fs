module GameGlobalState

open ScoreboardModel

type GameGlobalState =
    {
        GameScoreBoard  :  ScoreAndName list
    }

let InitialGameGlobals () =
    {
        GameScoreBoard = InitialScoreboard [ "Norman" ; "Scott" ; "Lara" ; "Mr J" ] 5000u 1000u
    }
