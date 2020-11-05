module Rules

open Time

let MaxPlayerNameLength = 10

let NoScore = 0u

// TODO: Could some of these parameters be per-ghost?

let ScoreForEatingDot       = 10u
let ScoreForEatingPowerPill = 250u
let ScoreForEatingGhost     = 500u

let PowerPillTime = 12.0F<seconds>
let PowerPillWarnTime = 4.0F<seconds>  // Must be less than PowerPillTime
let PowerPillWarnFlashRate = 12.0F

let RegenerationTime = 5.0F<seconds>
let RegenerationFlashRate = 8.0F

let PacmanDyingAnimationTime = 3.0F<seconds>
let PacmanDyingFlashRate = 32.0F
let PacmanDeadPauseTime = 3.0F<seconds>
