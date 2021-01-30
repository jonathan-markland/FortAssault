module Rules

open Time
open ScoreHiScore

let MaxPlayerNameLength = 10

let NoScore = 0u

let InitialLives = 3u

type BetweenScreenStatus =
    {
        ScoreAndHiScore : ScoreAndHiScore
        Lives           : uint32
    }

let GameOverPauseTime            = 5.0F<seconds>
let LifeOverGetReadyPauseTime    = 3.0F<seconds>
let ScreenCompleteStatusCardTime = 3.0F<seconds>
