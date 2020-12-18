module ScoreHiScore

type ScoreAndHiScore =
    {
        Score:    uint32
        HiScore:  uint32
    }

let ScoreIncrementedBy n { Score=oldScore ; HiScore=oldHiScore } =

    let newScore = oldScore + n

    {
        Score    = newScore
        HiScore  = max newScore oldHiScore
    }
