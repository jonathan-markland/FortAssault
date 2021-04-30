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

// TODO: Rename these "times" as "durations".
// TODO: Could some of these parameters be per-ghost?

let ScoreForEatingDot       = 10u
let ScoreForEatingPowerPill = 250u
let ScoreForEatingGhost     = 500u
let ScoreDeltaForExtraLife  = 10000u

let PowerPillTime           = 12.0<seconds>
let PowerPillWarnTime       = 4.0<seconds>  // Must be less than PowerPillTime
let PowerPillWarnFlashRate  = 12.0
let EdibleFlashRate         = 6.0

let RegenerationTime        = 5.0<seconds>
let RegenerationFlashRate   = 8.0

let PacmanDyingAnimationTime = 2.0<seconds> 
let PacmanDyingFlashRate     = 32.0
let PacmanDeadPauseTime      = 3.0<seconds>

let GetReadyCardTime             = 3.0<seconds>  // Must align with 321 voice sound effect
let LifeOverGetReadyPauseTime    = 4.0<seconds>
let ScreenCompletePauseTime      = 3.0<seconds>
let ScreenCompleteStatusCardTime = 6.0<seconds>
let GameOverPauseTime            = 5.0<seconds>

let SnapsPerSecond        = 8.0
let EyesTwitchesPerSecond = 2.0
