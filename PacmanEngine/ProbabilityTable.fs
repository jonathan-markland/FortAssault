module ProbabilityTable

open GhostDirectionChoosing  // for DirectionChoiceProbabilities
open MazeFilter              // for direction bitmasks



let IsValidBitmask railsBitmask = 
    ((railsBitmask &&& 15uy) = railsBitmask)



let RotateRailsClockwise railsBitmask =
    System.Diagnostics.Debug.Assert (railsBitmask |> IsValidBitmask)
    (((railsBitmask <<< 4) ||| railsBitmask) >>> 1) &&& 15uy



let RotateProbabilitiesClockwise probs =
    let { ProbLeft=pl ; ProbUp=pu ; ProbRight=pr ; ProbDown=pd } = probs
    { ProbLeft=pd ; ProbUp=pl ; ProbRight=pu ; ProbDown=pr }



type MovementProbabilitiesTableRow =
    {
        CaseDescription    : string
        Rails              : byte
        EntryDirectionMask : byte
        // ListIndex          : int
        MoveProbLeft       : byte
        MoveProbUp         : byte
        MoveProbRight      : byte
        MoveProbDown       : byte
    }



let IterMovementProbabilitiesTable action =

    let apply caseName primaryRailsMask (probList:DirectionChoiceProbabilities list) =
        
        let validateAgainstPrimaryRailsMask probs =
            let check directionName probValue directionMask =
                if (directionMask &&& primaryRailsMask) = 0uy then
                    if probValue <> 0uy then failwith (sprintf "The probability value for %s direction must be 0 to be consistent with the rails." directionName)
                else
                    if probValue = 0uy then failwith (sprintf "The probability value for %s direction must be non-zero to be consistent with the rails." directionName)
            let { ProbLeft=pl ; ProbUp=pu ; ProbRight=pr ; ProbDown=pd } = probs
            check "left"  pl MazeByteLeft 
            check "up"    pu MazeByteUp
            check "right" pr MazeByteRight
            check "down"  pd MazeByteDown

        probList |> List.iter validateAgainstPrimaryRailsMask

        let listLen = probList.Length

        let rot = RotateRailsClockwise
        let rotprb = RotateProbabilitiesClockwise

        let applyOrientation railsRotator probsRotator =

            let rails = primaryRailsMask |> railsRotator

            let resultsForEntryDirection dirIndex dirMask =
                let listIdx = dirIndex % listLen
                let probs = probList.[listIdx] |> probsRotator
                let { ProbLeft=pl ; ProbUp=pu ; ProbRight=pr ; ProbDown=pd } = probs
                // printfn "(%s):  Where rails=%d and entering from %d : list[%d] { LP=%d UP=%d RP=%d DP=%d }" caseName rails dirMask listIdx pl pu pr pd
                action 
                    {
                        CaseDescription    = caseName
                        Rails              = rails
                        EntryDirectionMask = dirMask
                        // ListIndex          = listIdx
                        MoveProbLeft       = pl
                        MoveProbUp         = pu
                        MoveProbRight      = pr
                        MoveProbDown       = pd
                    }
            
            resultsForEntryDirection  0 MazeByteLeft 
            resultsForEntryDirection  1 MazeByteUp
            resultsForEntryDirection  2 MazeByteRight
            resultsForEntryDirection  3 MazeByteDown

        applyOrientation  (id)                 (id)

        if primaryRailsMask <> 15uy then  // a little bit of a hack!
            applyOrientation  (rot)                (rotprb)
            applyOrientation  (rot >> rot)         (rotprb >> rotprb)
            applyOrientation  (rot >> rot >> rot)  (rotprb >> rotprb >> rotprb)


    apply
        "Single direction"
        (MazeByteUp)
        [
            { ProbLeft=0uy ; ProbUp=1uy ; ProbRight=0uy ; ProbDown=0uy }   // Entry facing any way
        ]
    
    apply 
        "Corners"
        (MazeByteUp + MazeByteRight) 
        [
            { ProbLeft=0uy ; ProbUp=9uy ; ProbRight=1uy ; ProbDown=0uy }   // Entry facing left
            { ProbLeft=0uy ; ProbUp=5uy ; ProbRight=5uy ; ProbDown=0uy }   // Entry facing up         [SELF-CORRECTIONAL]
            { ProbLeft=0uy ; ProbUp=5uy ; ProbRight=5uy ; ProbDown=0uy }   // Entry facing right      [SELF-CORRECTIONAL]
            { ProbLeft=0uy ; ProbUp=1uy ; ProbRight=9uy ; ProbDown=0uy }   // Entry facing down
        ]

    apply
        "Straights"
        (MazeByteUp + MazeByteDown) 
        [
            { ProbLeft=0uy ; ProbUp=5uy ; ProbRight=0uy ; ProbDown=5uy }   // Entry facing left   // Entry facing right   [SELF-CORRECTIONAL]
            { ProbLeft=0uy ; ProbUp=9uy ; ProbRight=0uy ; ProbDown=1uy }   // Entry facing up     // Entry facing down
        ]

    apply
        "Three-ways"
        (MazeByteUp + MazeByteLeft + MazeByteRight) 
        [
            { ProbLeft=6uy ; ProbUp=3uy ; ProbRight=1uy ; ProbDown=0uy }   // Entry facing left
            { ProbLeft=5uy ; ProbUp=5uy ; ProbRight=5uy ; ProbDown=0uy }   // Entry facing up       [SELF-CORRECTIONAL]
            { ProbLeft=1uy ; ProbUp=3uy ; ProbRight=6uy ; ProbDown=0uy }   // Entry facing right
            { ProbLeft=4uy ; ProbUp=2uy ; ProbRight=4uy ; ProbDown=0uy }   // Entry facing down
        ]

    apply
        "Four-ways"
        (MazeByteUp + MazeByteDown + MazeByteLeft + MazeByteRight) 
        [
            { ProbLeft=6uy ; ProbUp=3uy ; ProbRight=3uy ; ProbDown=1uy }   // Entry facing left
            { ProbLeft=3uy ; ProbUp=6uy ; ProbRight=3uy ; ProbDown=1uy }   // Entry facing up
            { ProbLeft=1uy ; ProbUp=3uy ; ProbRight=6uy ; ProbDown=3uy }   // Entry facing right
            { ProbLeft=3uy ; ProbUp=1uy ; ProbRight=3uy ; ProbDown=6uy }   // Entry facing down
        ]


        // TODO: Indirection needed , don't use the ProbLeft(etc) record type, generate an Abstract Table
        // TODO: Calculate four table copies per-ghost from the Abstract Table

let MovementProbabilitiesTable () =

    let mutable mutableTable = []
    IterMovementProbabilitiesTable (fun row -> mutableTable <- row::mutableTable)
    mutableTable
    
