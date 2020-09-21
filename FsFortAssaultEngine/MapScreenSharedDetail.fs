module MapScreenSharedDetail

open Time
open Geometry
open Mechanics
open Input
open InputEventData

/// When allied-enemy separation distance is this or less, we engage battle.
let EnemyEngagementDistance = 20.0F<epx>

let AlliedSpeed          = 0.3F<epx>
let EnemySpeed           = 0.3F<epx>

let DefaultEnemyFleetLocation  = { ptx= 97.0F<epx> ; pty=129.0F<epx> }

let PauseTimeOnceEngaged = 2.0F<seconds>


// The trigger rectangles must at least partially overlap the travel rectangles.

let SecretPassageTriggerRectangle =
    {
        Left   = 134.0F<epx>
        Top    =  35.0F<epx>
        Right  = 147.0F<epx>
        Bottom =  47.0F<epx>
    }

let EnemyGivesChaseTriggerRectangle =
    {
        Left   =  86.0F<epx>
        Top    =  75.0F<epx>
        Right  = 238.0F<epx>
        Bottom = 139.0F<epx>
    }

let BeachLandingTriggerRectangle =
    {
        Left   =  68.0F<epx>
        Top    = 130.0F<epx>
        Right  =  91.0F<epx>
        Bottom = 141.0F<epx>
    }

let MovementDeltaForInput speed input =

    let moveDelta leftKey rightKey =
        if leftKey.Held && rightKey.Held then
            0.0F<epx>
        elif leftKey.Held then
            -speed
        elif rightKey.Held then
            speed
        else
            0.0F<epx>

    {
        modx = moveDelta input.Left input.Right
        mody = moveDelta input.Up input.Down
    }
        
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewAlliedFleetLocation oldAlliesLocation input permissableTravelLocationRectangles =

    let newLocation = oldAlliesLocation |> PointMovedByDelta (MovementDeltaForInput AlliedSpeed input)

    if newLocation |> LiesWithinRectangleList permissableTravelLocationRectangles then
        newLocation
    else
        oldAlliesLocation

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let AttractionPointForEnemyFleet alliesLocation =

    if alliesLocation |> IsPointWithinRectangle EnemyGivesChaseTriggerRectangle then
        alliesLocation
    else
        DefaultEnemyFleetLocation

let NewEnemyFleetLocation oldEnemyLocation alliesLocation =

    let attractionPoint = AttractionPointForEnemyFleet alliesLocation

    if oldEnemyLocation |> IsWithinRegionOf attractionPoint 1.0F<epx> then
        oldEnemyLocation
    else
        let delta = oldEnemyLocation |> SimpleMovementDeltaToGetTo attractionPoint EnemySpeed
        oldEnemyLocation |> PointMovedByDelta delta




