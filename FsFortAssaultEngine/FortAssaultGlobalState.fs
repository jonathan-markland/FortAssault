module FortAssaultGlobalState

open ScoreboardModel

/// Contains everything that survives after a game is over.
type FortAssaultGlobalState =
    {
        GameScoreBoard  :  ScoreAndName list
    }

/// Establish the "boot-time" state for the scoreboard.
let FortAssaultGlobalStateConstructor () =
    {
        GameScoreBoard = InitialScoreboard [ "Bob" ; "Scott" ; "Lara" ; "J" ] 10000u 5000u
    }
