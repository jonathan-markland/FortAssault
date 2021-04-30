module Rules

open Time
open ScoreHiScore

let MaxPlayerNameLength = 10

let NoScore = 0u
let ScoreForPlayerHittingDroid = 10u

let InitialLives = 3u

type BetweenScreenStatus =
    {
        ScoreAndHiScore : ScoreAndHiScore
        Lives           : uint32
    }

let GameOverPauseTime            = 5.0<seconds>
let LifeOverGetReadyPauseTime    = 3.0<seconds>
let ScreenCompleteStatusCardTime = 3.0<seconds>
let ExplosionDuration            = 0.25<seconds>

