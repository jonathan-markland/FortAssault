﻿module GameGlobalState

open ScoreboardModel

/// Contains everything that survives after a game is over.
type GameGlobalState =
    {
        GameScoreBoard  :  ScoreAndName list
    }

/// Establish the "boot-time" state for the scoreboard.
let InitialGameGlobals () =
    {
        GameScoreBoard = InitialScoreboard [ "Bob" ; "Scott" ; "Lara" ; "J" ] 10000u 5000u
    }
