module GhostDirectionChoosing


/// The probability of turning through an angle with respect
/// to the current direction of travel.
[<Struct>]
type TurnProbability =
    {
        /// When at a 90 degree corner, the probability 
        /// of carrying on around the corner, out of 100.
        CornerProbTurn : byte

        /// When at a 3 or 4 way, the probability 
        /// of going ahead.  (Unless not available in
        /// which case this value is removed from the 
        /// probability total).
        ProbAhead    : byte

        /// When at a 3 or 4 way, the probability 
        /// of turning 90 degrees.  (Unless not available in
        /// which case this value is removed from the 
        /// probability total).
        ProbTurn90   : byte

        /// When at a 3 or 4 way, the probability 
        /// of turning back.  (Unless not available in
        /// which case this value is removed from the 
        /// probability total).
        ProbTurn180  : byte
    }


/// The probabilities associated with choosing 
/// a particular direction of travel.  These will
/// be zero if the direction CANNOT be chosen (wall)
/// or has been filtered out.
[<Struct>]
type DirectionChoiceProbabilities =
    {
        ProbLeft  : byte
        ProbUp    : byte
        ProbRight : byte
        ProbDown  : byte
    }


/// Pre-calculate DirectionChoiceProbabilities for each of the
/// four directions of travel.
let CalculateMemoizedDirectionProbabilities  turnProbabilities =

    let ahead = turnProbabilities.ProbAhead
    let turn  = turnProbabilities.ProbTurn90
    let rev   = turnProbabilities.ProbTurn180

    let arrangement  left up right down =
        {
            ProbLeft  = left
            ProbUp    = up
            ProbRight = right
            ProbDown  = down
        }

    [|
        //           Left   Up     Right  Down
        //          -----------------------------
        arrangement  ahead  turn   rev    turn    // FacingLeft
        arrangement  turn   ahead  turn   rev     // FacingUp
        arrangement  rev    turn   ahead  turn    // FacingRight
        arrangement  turn   rev    turn   ahead   // FacingDown
    |]


