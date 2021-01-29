module GhostAI

open MazeFilter
open Directions


/// Decision making probability traits for a ghost.
type GhostMoveTraits =
    {
        Description : string

        /// The probability of turning through a 90-degree corner.
        Turn90 : int

        /// The probability of turning back at a 90-degree corner.
        Back90 : int    

        /// The probability of heading onwards at a straight.
        Onward180 : int 

        /// The probability of turning back at a straight.
        Back180 : int

        /// The probability of heading onwards 
        /// at 3-way of appropriate orientation.
        OnwardT    : int

        /// The probability of turning 90 degrees
        /// at 3-way that has a straight-ahead option.
        TurnAwayT  : int

        /// The probability of turning back
        /// at 3-way that has a straight-ahead option.
        BackT : int

        /// The probability of turning 90 degrees
        /// at 3-way without a straight-ahead option.
        TurnIntoT : int

        /// The probability of turning back
        /// at 3-way without a straight-ahead option.
        TurnBackT  : int

        /// The probability of heading onwards at a 4-way.
        Onward4    : int

        /// The probability of turning 90 degrees left or right at a 4-way.
        Turn4      : int

        /// The probability of turning back at a 4-way.
        Back4 : int
    }



/// Obtain ghost direction probabilities with three 
/// levels of decisiveness.
let GetGhostMoveTraits () =

    let charger =
        {
            Description = "Charger"

            // Corner
            Turn90 = 100
            Back90 = 1      // NB: If we make this 0 then he will never turn to "get" pacman

            /// Straight
            Onward180 = 100
            Back180 = 0  // NB: 0 to avoid bouncing when "seeing" other ghosts on straight sections.

            /// 3-way that has a straight-ahead option
            OnwardT    = 80
            TurnAwayT  = 20
            BackT      = 1    // NB: If we make this 0 then he will never turn to "get" pacman

            /// 3-way without a straight-ahead option
            TurnIntoT = 50
            TurnBackT = 1     // NB: If we make this 0 then he will never turn to "get" pacman

            /// 4-way
            Onward4 = 80
            Turn4   = 10
            Back4   = 1       // NB: If we make this 0 then he will never turn to "get" pacman
        }

    let standard =
        {
            Description = "Standard"

            // Corner
            Turn90 = 95
            Back90 = 5

            /// Straight
            Onward180 = 99
            Back180 = 0  // NB: 0 to avoid bouncing when "seeing" other ghosts on straight sections.

            /// 3-way that has a straight-ahead option
            OnwardT    = 70
            TurnAwayT  = 20
            BackT      = 10

            /// 3-way without a straight-ahead option
            TurnIntoT = 45
            TurnBackT = 10

            /// 4-way
            Onward4 = 30
            Turn4   = 30
            Back4   = 10
        }

    let ditherer =
        {
            Description = "Ditherer"
    
            // Corner
            Turn90 = 60
            Back90 = 40

            /// Straight
            Onward180 = 95
            Back180 = 0  // NB: 0 to avoid bouncing when "seeing" other ghosts on straight sections.

            /// 3-way that has a straight-ahead option
            OnwardT    = 30
            TurnAwayT  = 30
            BackT      = 30

            /// 3-way without a straight-ahead option
            TurnIntoT = 30
            TurnBackT = 30

            /// 4-way
            Onward4 = 20
            Turn4   = 20
            Back4   = 20
        }

    (charger, standard, ditherer)



type ProbabilityClass = 

    /// It is never allowed to turn in this direction
    /// as it would invalidate the rails.
    /// This is handled by the system as probability=0.
    | Never

    /// The entry direction is inconsistent with the
    /// rails, and so we could not determine an entry
    /// direction.  So, we bail and randomly select
    /// the alternatives with equal weighting.
    /// (Initial heading could be inconsistent with the
    /// rails at the start of a level, or after ghost
    /// returns to base).
    /// Will not appear on a row with anything else other
    /// than Correct or Never.
    /// This is handled by the system as probability=1.
    | Correct    

    /// The probability of taking the only direction
    /// that is available is 1.
    | OnlyWay 

    /// The probability of turning through a 90-degree corner.
    | Turn90 
    
    /// The probability of turning back at a 90-degree corner.
    | Back90    

    /// The probability of heading onwards at a straight.
    | Onward180 
    
    /// The probability of turning back at a straight.
    | Back180

    /// The probability of heading onwards 
    /// at 3-way of appropriate orientation.
    | OnwardT   
    
    /// The probability of turning 90 degrees
    /// at 3-way that has a straight-ahead option.
    | TurnAwayT 
    
    /// The probability of turning back
    /// at 3-way that has a straight-ahead option.
    | BackT
    
    /// The probability of turning 90 degrees
    /// at 3-way without a straight-ahead option.
    | TurnIntoT

    /// The probability of turning back
    /// at 3-way without a straight-ahead option.
    | TurnBackT 

    /// The probability of heading onwards at a 4-way.
    | Onward4   
    
    /// The probability of turning 90 degrees left or right at a 4-way.
    | Turn4     
    
    /// The probability of turning back at a 4-way.
    | Back4



type ProbabilityClassesForDirections =
    { 
        PCLeft  : ProbabilityClass
        PCUp    : ProbabilityClass
        PCRight : ProbabilityClass
        PCDown  : ProbabilityClass
    } 



[<Struct>]
type DirectionProbability = DirectionProbability of byte

let inline dp (i:int) = DirectionProbability ((byte) i)



let ProbClassToValue ghostMoveTraits probClass =
    
    let i = 
        match probClass with
            | Never     -> 0
            | Correct   -> 1
            | OnlyWay   -> 1
            | Turn90    -> ghostMoveTraits.Turn90
            | Back90    -> ghostMoveTraits.Back90
            | Onward180 -> ghostMoveTraits.Onward180
            | Back180   -> ghostMoveTraits.Back180  
            | OnwardT   -> ghostMoveTraits.OnwardT  
            | TurnAwayT -> ghostMoveTraits.TurnAwayT
            | BackT     -> ghostMoveTraits.BackT    
            | TurnIntoT -> ghostMoveTraits.TurnIntoT
            | TurnBackT -> ghostMoveTraits.TurnBackT
            | Onward4   -> ghostMoveTraits.Onward4  
            | Turn4     -> ghostMoveTraits.Turn4    
            | Back4     -> ghostMoveTraits.Back4    

    let b = (byte) i
    
    if ((int) b) <> i then 
        failwith "Out of range probability value -- must fit a byte"
    else
        DirectionProbability b




/// The probabilities associated with choosing 
/// a particular direction of travel.  These will
/// be zero if the direction CANNOT be chosen (wall)
/// or has been filtered out.
[<Struct>]
type DirectionChoiceProbabilities =
    {
        ProbLeft  : DirectionProbability
        ProbUp    : DirectionProbability
        ProbRight : DirectionProbability
        ProbDown  : DirectionProbability
    }

let HasMoreThanOnePossibleDirection directionProbs =
    let test (DirectionProbability(prob)) = if prob > 0uy then 1 else 0
    let l = test directionProbs.ProbLeft
    let u = test directionProbs.ProbUp
    let r = test directionProbs.ProbRight
    let d = test directionProbs.ProbDown
    (l + u + r + d) > 1

/// Obtains the probability of going in the direction specified.
let ProbOfDirection direction directions =
    match direction with
        | FacingLeft  -> directions.ProbLeft
        | FacingUp    -> directions.ProbUp
        | FacingRight -> directions.ProbRight
        | FacingDown  -> directions.ProbDown

let LeftOnly  = { ProbLeft=dp 1  ; ProbUp=dp 0  ; ProbRight=dp 0  ; ProbDown=dp 0 }
let UpOnly    = { ProbLeft=dp 0  ; ProbUp=dp 1  ; ProbRight=dp 0  ; ProbDown=dp 0 }
let RightOnly = { ProbLeft=dp 0  ; ProbUp=dp 0  ; ProbRight=dp 1  ; ProbDown=dp 0 }
let DownOnly  = { ProbLeft=dp 0  ; ProbUp=dp 0  ; ProbRight=dp 0  ; ProbDown=dp 1 }

let inline QuarterProb (DirectionProbability(n)) = 
    DirectionProbability (
        if n = 0uy then
            0uy
        else 
            max (n / 4uy) 1uy)

let WithReducedLeft  directions = { directions with ProbLeft  = QuarterProb directions.ProbLeft  }
let WithReducedUp    directions = { directions with ProbUp    = QuarterProb directions.ProbUp    }
let WithReducedRight directions = { directions with ProbRight = QuarterProb directions.ProbRight }
let WithReducedDown  directions = { directions with ProbDown  = QuarterProb directions.ProbDown  }



type GhostAI = GhostAI of DirectionChoiceProbabilities []



let GhostMovementTable ghostMoveTraits =

    let substitutedWith ghostMoveTraits probClasses =
        { 
            ProbLeft  = probClasses.PCLeft  |> ProbClassToValue ghostMoveTraits
            ProbUp    = probClasses.PCUp    |> ProbClassToValue ghostMoveTraits
            ProbRight = probClasses.PCRight |> ProbClassToValue ghostMoveTraits
            ProbDown  = probClasses.PCDown  |> ProbClassToValue ghostMoveTraits
        } 

    // The following table is designed for the following bitmask ordering and enum.
    // This will not be changed!

    assert (MazeByteLeft  = MazeByte 8uy)
    assert (MazeByteUp    = MazeByte 4uy)
    assert (MazeByteRight = MazeByte 2uy)
    assert (MazeByteDown  = MazeByte 1uy)

    assert ((FacingLeft  |> FacingDirectionToInt) = 0)
    assert ((FacingUp    |> FacingDirectionToInt) = 1)
    assert ((FacingRight |> FacingDirectionToInt) = 2)
    assert ((FacingDown  |> FacingDirectionToInt) = 3)

    // The following array is indexed by:
    // (rails bitmask (1-based) << 2 | Ghost's facing direction enumeration)
    // Where ghost facing direction is Left=0 Up=1 Right=2 Down=3

    let probabilitiesTemplate =
        [
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Never     ; PCDown=OnlyWay   } // '╷'  0.  1 Single direction | Entry facing left
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Never     ; PCDown=OnlyWay   } // '╷'  1.  1 Single direction | Entry facing up
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Never     ; PCDown=OnlyWay   } // '╷'  2.  1 Single direction | Entry facing right
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Never     ; PCDown=OnlyWay   } // '╷'  3.  1 Single direction | Entry facing down
            
            { PCLeft=Never     ; PCUp=Never     ; PCRight=OnlyWay   ; PCDown=Never     } // '╶'  4.  2 Single direction | Entry facing left
            { PCLeft=Never     ; PCUp=Never     ; PCRight=OnlyWay   ; PCDown=Never     } // '╶'  5.  2 Single direction | Entry facing up
            { PCLeft=Never     ; PCUp=Never     ; PCRight=OnlyWay   ; PCDown=Never     } // '╶'  6.  2 Single direction | Entry facing right
            { PCLeft=Never     ; PCUp=Never     ; PCRight=OnlyWay   ; PCDown=Never     } // '╶'  7.  2 Single direction | Entry facing down
            
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Back90    ; PCDown=Turn90    } // '┌'  8.  3 Corners | Entry facing left
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Turn90    ; PCDown=Back90    } // '┌'  9.  3 Corners | Entry facing up
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Correct   ; PCDown=Correct   } // '┌' 10.  3 Corners | Entry facing right
            { PCLeft=Never     ; PCUp=Never     ; PCRight=Correct   ; PCDown=Correct   } // '┌' 11.  3 Corners | Entry facing down
            
            { PCLeft=Never     ; PCUp=OnlyWay   ; PCRight=Never     ; PCDown=Never     } // '╵' 12.  4 Single direction | Entry facing left
            { PCLeft=Never     ; PCUp=OnlyWay   ; PCRight=Never     ; PCDown=Never     } // '╵' 13.  4 Single direction | Entry facing up
            { PCLeft=Never     ; PCUp=OnlyWay   ; PCRight=Never     ; PCDown=Never     } // '╵' 14.  4 Single direction | Entry facing right
            { PCLeft=Never     ; PCUp=OnlyWay   ; PCRight=Never     ; PCDown=Never     } // '╵' 15.  4 Single direction | Entry facing down
            
            { PCLeft=Never     ; PCUp=Correct   ; PCRight=Never     ; PCDown=Correct   } // '│' 16.  5 Straights | Entry facing left
            { PCLeft=Never     ; PCUp=Onward180 ; PCRight=Never     ; PCDown=Back180   } // '│' 17.  5 Straights | Entry facing up
            { PCLeft=Never     ; PCUp=Correct   ; PCRight=Never     ; PCDown=Correct   } // '│' 18.  5 Straights | Entry facing right
            { PCLeft=Never     ; PCUp=Back180   ; PCRight=Never     ; PCDown=Onward180 } // '│' 19.  5 Straights | Entry facing down
            
            { PCLeft=Never     ; PCUp=Turn90    ; PCRight=Back90    ; PCDown=Never     } // '└' 20.  6 Corners | Entry facing left
            { PCLeft=Never     ; PCUp=Correct   ; PCRight=Correct   ; PCDown=Never     } // '└' 21.  6 Corners | Entry facing up
            { PCLeft=Never     ; PCUp=Correct   ; PCRight=Correct   ; PCDown=Never     } // '└' 22.  6 Corners | Entry facing right
            { PCLeft=Never     ; PCUp=Back90    ; PCRight=Turn90    ; PCDown=Never     } // '└' 23.  6 Corners | Entry facing down
            
            { PCLeft=Never     ; PCUp=TurnIntoT ; PCRight=TurnBackT ; PCDown=TurnIntoT } // '├' 24.  7 Three-ways | Entry facing left
            { PCLeft=Never     ; PCUp=OnwardT   ; PCRight=TurnAwayT ; PCDown=BackT     } // '├' 25.  7 Three-ways | Entry facing up
            { PCLeft=Never     ; PCUp=Correct   ; PCRight=Correct   ; PCDown=Correct   } // '├' 26.  7 Three-ways | Entry facing right
            { PCLeft=Never     ; PCUp=BackT     ; PCRight=TurnAwayT ; PCDown=OnwardT   } // '├' 27.  7 Three-ways | Entry facing down
            
            { PCLeft=OnlyWay   ; PCUp=Never     ; PCRight=Never     ; PCDown=Never     } // '╴' 28.  8 Single direction | Entry facing left
            { PCLeft=OnlyWay   ; PCUp=Never     ; PCRight=Never     ; PCDown=Never     } // '╴' 29.  8 Single direction | Entry facing up
            { PCLeft=OnlyWay   ; PCUp=Never     ; PCRight=Never     ; PCDown=Never     } // '╴' 30.  8 Single direction | Entry facing right
            { PCLeft=OnlyWay   ; PCUp=Never     ; PCRight=Never     ; PCDown=Never     } // '╴' 31.  8 Single direction | Entry facing down
            
            { PCLeft=Correct   ; PCUp=Never     ; PCRight=Never     ; PCDown=Correct   } // '┐' 32.  9 Corners | Entry facing left
            { PCLeft=Turn90    ; PCUp=Never     ; PCRight=Never     ; PCDown=Back90    } // '┐' 33.  9 Corners | Entry facing up
            { PCLeft=Back90    ; PCUp=Never     ; PCRight=Never     ; PCDown=Turn90    } // '┐' 34.  9 Corners | Entry facing right
            { PCLeft=Correct   ; PCUp=Never     ; PCRight=Never     ; PCDown=Correct   } // '┐' 35.  9 Corners | Entry facing down
            
            { PCLeft=Onward180 ; PCUp=Never     ; PCRight=Back180   ; PCDown=Never     } // '─' 36. 10 Straights | Entry facing left
            { PCLeft=Correct   ; PCUp=Never     ; PCRight=Correct   ; PCDown=Never     } // '─' 37. 10 Straights | Entry facing up
            { PCLeft=Back180   ; PCUp=Never     ; PCRight=Onward180 ; PCDown=Never     } // '─' 38. 10 Straights | Entry facing right
            { PCLeft=Correct   ; PCUp=Never     ; PCRight=Correct   ; PCDown=Never     } // '─' 39. 10 Straights | Entry facing down
            
            { PCLeft=OnwardT   ; PCUp=Never     ; PCRight=BackT     ; PCDown=TurnAwayT } // '┬' 40. 11 Three-ways | Entry facing left
            { PCLeft=TurnIntoT ; PCUp=Never     ; PCRight=TurnIntoT ; PCDown=TurnBackT } // '┬' 41. 11 Three-ways | Entry facing up
            { PCLeft=BackT     ; PCUp=Never     ; PCRight=OnwardT   ; PCDown=TurnAwayT } // '┬' 42. 11 Three-ways | Entry facing right
            { PCLeft=Correct   ; PCUp=Never     ; PCRight=Correct   ; PCDown=Correct   } // '┬' 43. 11 Three-ways | Entry facing down
            
            { PCLeft=Correct   ; PCUp=Correct   ; PCRight=Never     ; PCDown=Never     } // '┘' 44. 12 Corners | Entry facing left
            { PCLeft=Correct   ; PCUp=Correct   ; PCRight=Never     ; PCDown=Never     } // '┘' 45. 12 Corners | Entry facing up
            { PCLeft=Back90    ; PCUp=Turn90    ; PCRight=Never     ; PCDown=Never     } // '┘' 46. 12 Corners | Entry facing right
            { PCLeft=Turn90    ; PCUp=Back90    ; PCRight=Never     ; PCDown=Never     } // '┘' 47. 12 Corners | Entry facing down
            
            { PCLeft=Correct   ; PCUp=Correct   ; PCRight=Never     ; PCDown=Correct   } // '┤' 48. 13 Three-ways | Entry facing left
            { PCLeft=TurnAwayT ; PCUp=OnwardT   ; PCRight=Never     ; PCDown=BackT     } // '┤' 49. 13 Three-ways | Entry facing up
            { PCLeft=TurnBackT ; PCUp=TurnIntoT ; PCRight=Never     ; PCDown=TurnIntoT } // '┤' 50. 13 Three-ways | Entry facing right
            { PCLeft=TurnAwayT ; PCUp=BackT     ; PCRight=Never     ; PCDown=OnwardT   } // '┤' 51. 13 Three-ways | Entry facing down
            
            { PCLeft=OnwardT   ; PCUp=TurnAwayT ; PCRight=BackT     ; PCDown=Never     } // '┴' 52. 14 Three-ways | Entry facing left
            { PCLeft=Correct   ; PCUp=Correct   ; PCRight=Correct   ; PCDown=Never     } // '┴' 53. 14 Three-ways | Entry facing up
            { PCLeft=BackT     ; PCUp=TurnAwayT ; PCRight=OnwardT   ; PCDown=Never     } // '┴' 54. 14 Three-ways | Entry facing right
            { PCLeft=TurnIntoT ; PCUp=TurnBackT ; PCRight=TurnIntoT ; PCDown=Never     } // '┴' 55. 14 Three-ways | Entry facing down
            
            { PCLeft=Onward4   ; PCUp=Turn4     ; PCRight=Turn4     ; PCDown=Back4     } // '┼' 56. 15 Four-ways | Entry facing left
            { PCLeft=Turn4     ; PCUp=Onward4   ; PCRight=Turn4     ; PCDown=Back4     } // '┼' 57. 15 Four-ways | Entry facing up
            { PCLeft=Back4     ; PCUp=Turn4     ; PCRight=Onward4   ; PCDown=Turn4     } // '┼' 58. 15 Four-ways | Entry facing right
            { PCLeft=Turn4     ; PCUp=Back4     ; PCRight=Turn4     ; PCDown=Onward4   } // '┼' 59. 15 Four-ways | Entry facing down
        ]

    probabilitiesTemplate 
        |> List.map (substitutedWith ghostMoveTraits)
        |> List.toArray
        |> GhostAI
        // TODO: Validate that the probabilities do NOT violate the rails.



let GetDirectionProbabilities facingDirection railsBitmask ghostAI =

    let (GhostAI(table)) = ghostAI
    System.Diagnostics.Debug.Assert (railsBitmask |> IsNonEmpty4way)
    
    let dirInt = facingDirection |> FacingDirectionToInt
    System.Diagnostics.Debug.Assert (dirInt >= 0 && dirInt <= 3)

    let (MazeByte railsAsByte) = railsBitmask
    let index = dirInt ||| ((int)((railsAsByte - 1uy)) <<< 2)

    // Debug tracing where corrections are made.
    // Will happen where ghosts heading is inconsistent with the rails,
    // which itself, can only happen at the start of a level (initial
    // heading inconsistent; or after a return-to-base).
    // if [10;11;16;18;21;22;26;32;35;37;39;43;44;45;48;53] |> List.contains index then
    //     System.Diagnostics.Trace.WriteLine (sprintf "Correction index %d" index)

    table.[index]

