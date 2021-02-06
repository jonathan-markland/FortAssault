module ScoreHiScore

type ScoreAndHiScore =
    {
        Score:    uint32
        HiScore:  uint32
    }

let ScoreIncrementedBy n scoreAndHiScore =

    if n <> 0u then

        let { Score=oldScore ; HiScore=oldHiScore } = scoreAndHiScore
        let newScore = oldScore + n
        {
            Score    = newScore
            HiScore  = max newScore oldHiScore
        }

    else scoreAndHiScore
