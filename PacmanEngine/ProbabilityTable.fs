module ProbabilityTable

open GhostDirectionChoosing  // for DirectionChoiceProbabilities
open MazeFilter              // for direction bitmasks
open GhostMoveTraits




let IsValidBitmask railsBitmask = 
    ((railsBitmask &&& 15uy) = railsBitmask)



let RotateRailsClockwise railsBitmask =
    System.Diagnostics.Debug.Assert (railsBitmask |> IsValidBitmask)
    (((railsBitmask <<< 4) ||| railsBitmask) >>> 1) &&& 15uy



let RotateProbabilitiesClockwise probs =
    let { PCLeft=pl ; PCUp=pu ; PCRight=pr ; PCDown=pd } = probs
    { PCLeft=pd ; PCUp=pl ; PCRight=pu ; PCDown=pr }



type MovementProbabilitiesTableRow =
    {
        CaseDescription    : string
        Rails              : byte
        EntryDirectionMask : byte  // 8,4,2 or 1 for L,U,R,D resp
        Probs              : ProbabilityClassesForDirections
    }



let IterMovementProbabilitiesTable action =

    let apply caseName primaryRailsMask (probList:ProbabilityClassesForDirections list) =
        
        let validateAgainstPrimaryRailsMask probs =
            let check directionName probValue directionMask =
                if (directionMask &&& primaryRailsMask) = 0uy then
                    if probValue <> Never then failwith (sprintf "The probability value for %s direction must be 0 to be consistent with the rails." directionName)
                else
                    if probValue = Never then failwith (sprintf "The probability value for %s direction must be non-zero to be consistent with the rails." directionName)
            
            let { PCLeft=pl ; PCUp=pu ; PCRight=pr ; PCDown=pd } = probs
            
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
                let { PCLeft=pl ; PCUp=pu ; PCRight=pr ; PCDown=pd } = probs
                // printfn "(%s):  Where rails=%d and entering from %d : list[%d] { LP=%d UP=%d RP=%d DP=%d }" caseName rails dirMask listIdx pl pu pr pd
                action 
                    {
                        CaseDescription    = caseName
                        Rails              = rails
                        EntryDirectionMask = dirMask
                        Probs              = probs
                    }
            
            resultsForEntryDirection  0 MazeByteLeft 
            resultsForEntryDirection  1 MazeByteUp
            resultsForEntryDirection  2 MazeByteRight
            resultsForEntryDirection  3 MazeByteDown

        applyOrientation  (id)                 (id)

        if primaryRailsMask <> 15uy then  // a little bit of a hack!
            applyOrientation  (rot)                (rotprb)
            
            if primaryRailsMask <> 5uy && primaryRailsMask <> 10uy then  // a little bit of a hack!
                applyOrientation  (rot >> rot)         (rotprb >> rotprb)
                applyOrientation  (rot >> rot >> rot)  (rotprb >> rotprb >> rotprb)


    apply
        "Single direction"
        (MazeByteUp)
        [
            { PCLeft=Never ; PCUp=OnlyWay ; PCRight=Never ; PCDown=Never }   // Entry facing any way
        ]
    
    apply 
        "Corners"
        (MazeByteUp + MazeByteRight) 
        [
            { PCLeft=Never ; PCUp=Turn90  ; PCRight=Back90  ; PCDown=Never }   // Entry facing left
            { PCLeft=Never ; PCUp=Correct ; PCRight=Correct ; PCDown=Never }   // Entry facing up         [SELF-CORRECTIONAL]
            { PCLeft=Never ; PCUp=Correct ; PCRight=Correct ; PCDown=Never }   // Entry facing right      [SELF-CORRECTIONAL]
            { PCLeft=Never ; PCUp=Back90  ; PCRight=Turn90  ; PCDown=Never }   // Entry facing down
        ]

    apply
        "Straights"
        (MazeByteUp + MazeByteDown) 
        [
            { PCLeft=Never ; PCUp=Correct   ; PCRight=Never ; PCDown=Correct }   // Entry facing left   // Entry facing right   [SELF-CORRECTIONAL]
            { PCLeft=Never ; PCUp=Onward180 ; PCRight=Never ; PCDown=Back180 }   // Entry facing up     // Entry facing down
        ]

    apply
        "Three-ways"
        (MazeByteUp + MazeByteLeft + MazeByteRight) 
        [
            { PCLeft=OnwardT   ; PCUp=TurnAwayT ; PCRight=BackT     ; PCDown=Never }   // Entry facing left
            { PCLeft=Correct   ; PCUp=Correct   ; PCRight=Correct   ; PCDown=Never }   // Entry facing up       [SELF-CORRECTIONAL]
            { PCLeft=BackT     ; PCUp=TurnAwayT ; PCRight=OnwardT   ; PCDown=Never }   // Entry facing right
            { PCLeft=TurnIntoT ; PCUp=TurnBackT ; PCRight=TurnIntoT ; PCDown=Never }   // Entry facing down
        ]

    apply
        "Four-ways"
        (MazeByteUp + MazeByteDown + MazeByteLeft + MazeByteRight) 
        [
            { PCLeft=Onward4 ; PCUp=Turn4   ; PCRight=Turn4   ; PCDown=Back4   }   // Entry facing left
            { PCLeft=Turn4   ; PCUp=Onward4 ; PCRight=Turn4   ; PCDown=Back4   }   // Entry facing up
            { PCLeft=Back4   ; PCUp=Turn4   ; PCRight=Onward4 ; PCDown=Turn4   }   // Entry facing right
            { PCLeft=Turn4   ; PCUp=Back4   ; PCRight=Turn4   ; PCDown=Onward4 }   // Entry facing down
        ]


        // TODO: Indirection needed , don't use the ProbLeft(etc) record type, generate an Abstract Table
        // TODO: Calculate four table copies per-ghost from the Abstract Table

let MovementProbabilitiesTable () =

    let mutable mutableTable = []
    IterMovementProbabilitiesTable (fun row -> mutableTable <- row::mutableTable)
    mutableTable
    
