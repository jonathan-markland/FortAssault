module Rules

open Time
open ScoreHiScore

let MaxPlayerNameLength = 10

let NoScore = 0u

let InitialLives = 1u // TODO

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

let PowerPillTime = 12.0F<seconds>
let PowerPillWarnTime = 4.0F<seconds>  // Must be less than PowerPillTime
let PowerPillWarnFlashRate = 12.0F
let EdibleFlashRate = 6.0F

let RegenerationTime = 5.0F<seconds>
let RegenerationFlashRate = 8.0F

let PacmanDyingAnimationTime = 2.0F<seconds> 
let PacmanDyingFlashRate = 32.0F
let PacmanDeadPauseTime = 3.0F<seconds>

let GetReadyCardTime = 2.0F<seconds>
let LifeOverGetReadyPauseTime = 4.0F<seconds>
let ScreenCompletePauseTime = 3.0F<seconds>
let ScreenCompleteStatusCardTime = 6.0F<seconds>
let GameOverPauseTime = 5.0F<seconds>

let SnapsPerSecond        = 8.0F
let EyesTwitchesPerSecond = 2.0F
