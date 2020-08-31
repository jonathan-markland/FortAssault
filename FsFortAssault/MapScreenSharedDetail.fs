module MapScreenSharedDetail

open Time
open Geometry
open Mechanics
open Input
open InputEventData

/// When allied-enemy separation distance is this or less, we engage battle.
let EnemyEngagementDistance = 20.0F<wu>

let AlliedSpeed          = 0.3F<wu>
let EnemySpeed           = 0.3F<wu>

let DefaultEnemyFleetLocation  = { xwf= 97.0F<wu> ; ywf=129.0F<wu> }

let PauseTimeOnceEngaged = 2.0F<seconds>


// The trigger rectangles must at least partially overlap the travel rectangles.

let SecretPassageTriggerRectangle =
    {
        Left   = 134.0F<wu>
        Top    =  35.0F<wu>
        Right  = 147.0F<wu>
        Bottom =  47.0F<wu>
    }

let EnemyGivesChaseTriggerRectangle =
    {
        Left   =  86.0F<wu>
        Top    =  75.0F<wu>
        Right  = 238.0F<wu>
        Bottom = 139.0F<wu>
    }

let BeachLandingTriggerRectangle =
    {
        Left   =  68.0F<wu>
        Top    = 130.0F<wu>
        Right  =  91.0F<wu>
        Bottom = 141.0F<wu>
    }

let MovementDeltaForInput speed input =

    let moveDelta leftKey rightKey =
        if leftKey.Held && rightKey.Held then
            0.0F<wu>
        elif leftKey.Held then
            -speed
        elif rightKey.Held then
            speed
        else
            0.0F<wu>

    {
        MovementDeltaX = moveDelta input.Left input.Right
        MovementDeltaY = moveDelta input.Up input.Down
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

    if oldEnemyLocation |> IsWithinRegionOf attractionPoint 1.0F<wu> then
        oldEnemyLocation
    else
        let delta = oldEnemyLocation |> SimpleMovementDeltaToGetTo attractionPoint EnemySpeed
        oldEnemyLocation |> PointMovedByDelta delta




