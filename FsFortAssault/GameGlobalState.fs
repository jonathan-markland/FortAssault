module GameGlobalState

open ScoreboardModel

type GameGlobalState =
    {
        GameScoreBoard  :  ScoreAndName list
    }

let InitialGameGlobals () =
    {
        GameScoreBoard = InitialScoreboard [ "Bob" ; "Scott" ; "Lara" ; "J" ] 5000u 1000u
    }
