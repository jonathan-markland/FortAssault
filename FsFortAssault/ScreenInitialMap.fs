module ScreenInitialMap

open Angle
open Time
open SharedDrawing
open DrawingCommandsEx
open ScoreHiScore
open Geometry
open ImagesAndFonts
open ScorePanel
open MapScreenSharedDetail
open Rules


let DefaultAlliedFleetLocation = { xwf=290.0F<wu> ; ywf=15.0F<wu> }

/// These are permitted to overlap other rectangles, including the trigger rectangles.
let PermissableTravelLocationRectangles =
    [
        {
            Left   =  38.0F<wu>
            Top    =   7.0F<wu>
            Right  = 310.0F<wu>
            Bottom =  19.0F<wu>
        }
        {
            Left   = 125.0F<wu>
            Top    =  19.0F<wu>
            Right  = 310.0F<wu>
            Bottom =  35.0F<wu>
        }
        {
            Left   = 133.0F<wu>
            Top    =  35.0F<wu>
            Right  = 310.0F<wu>
            Bottom =  41.0F<wu>
        }
        {
            Left   = 168.0F<wu>
            Top    =  41.0F<wu>
            Right  = 310.0F<wu>
            Bottom =  50.0F<wu>
        }
        {
            Left   = 200.0F<wu>
            Top    =  49.0F<wu>
            Right  = 310.0F<wu>
            Bottom = 139.0F<wu>
        }
        {
            Left   =  86.0F<wu>
            Top    =  75.0F<wu>
            Right  = 276.0F<wu>
            Bottom = 139.0F<wu>
        }
        SecretPassageTriggerRectangle
        BeachLandingTriggerRectangle
    ]

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type AlliedState =
    | MapInPlay of fleetLocation:PointF32
    | FleetEngagedOnMap of fleetLocation:PointF32 * pauseStartTime:float32<seconds> * stateAfterPause:AlliedState
    | ScreenOverEngagedSecretPassage
    | ScreenOverEngagedEnemyAtSea

type InitialMapScreenModel =
    {
        ScoreAndHiScore:      ScoreAndHiScore
        EnemyFleetCentre:     PointF32
        AlliedState:          AlliedState
        NumShips:             uint32
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let AlliesVersusSecretPassageOrEnemy alliesLocation enemyLocation gameTime =

    if alliesLocation |> IsPointWithinRectangle SecretPassageTriggerRectangle then
        FleetEngagedOnMap(alliesLocation, gameTime, ScreenOverEngagedSecretPassage)

    elif alliesLocation |> IsWithinRegionOf enemyLocation EnemyEngagementDistance then
        FleetEngagedOnMap(alliesLocation, gameTime, ScreenOverEngagedEnemyAtSea)

    else
        MapInPlay(alliesLocation)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderInitialMapScreen render (model:InitialMapScreenModel) =

    Image1to1 render 0<wu> 0<wu> ImageMap.ImageID

    // PermissableTravelLocationRectangles |> List.iteri (fun i r ->
    //     render (DrawFilledRectangle(r.Left, r.Top, r |> RectangleWidth, r |> RectangleHeight, i |> AlternateOf 0xEE0000u 0x00FF00u)))

    match model.AlliedState with
        | MapInPlay(location)
        | FleetEngagedOnMap(location, _, _) ->
            CentreImage render location.xwf location.ywf ImageAlliedFleetSymbol
            CentreImage render model.EnemyFleetCentre.xwf model.EnemyFleetCentre.ywf ImageEnemyFleetSymbol
        | ScreenOverEngagedEnemyAtSea
        | ScreenOverEngagedSecretPassage ->
            ()

    let mapHeight = ImageMap.ImageHeight

    ScoreboardArea render (mapHeight |> FloatWuToIntWu)

    let scorePanel =
        {
            ScoreAndHiScore  = model.ScoreAndHiScore
            ShipsPending     = 0u
            ShipsThrough     = model.NumShips
            Tanks            = model.NumShips |> ToTankCountFromShipCount
            Damage           = 0u
            Ammunition       = 10u
            Elevation        = 0.0F<degrees> // TODO:  Should this be optional?
        }

    DrawScorePanel render (mapHeight |> FloatWuToIntWu) scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewInitialMapScreen numShips scoreAndHiScore =
    {
        ScoreAndHiScore  = scoreAndHiScore
        AlliedState      = MapInPlay(DefaultAlliedFleetLocation)
        EnemyFleetCentre = DefaultEnemyFleetLocation
        NumShips         = numShips
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

[<Struct>]
type ChapterTransitionFromInitialMap =
    | StayOnInitialMap  of f1:InitialMapScreenModel
    | GoToSecretPassage of f2:InitialMapScreenModel
    | GoToBattleAtSea   of f3:InitialMapScreenModel

let NextInitialMapScreenState oldState input gameTime =

    let newState = 
        match oldState.AlliedState with

            | MapInPlay(alliedLocation) ->
    
                let alliedLocation =
                    NewAlliedFleetLocation alliedLocation input PermissableTravelLocationRectangles

                let enemyLocation =
                    NewEnemyFleetLocation oldState.EnemyFleetCentre alliedLocation

                let allies =
                    AlliesVersusSecretPassageOrEnemy alliedLocation enemyLocation gameTime

                {
                    ScoreAndHiScore  = oldState.ScoreAndHiScore
                    AlliedState      = allies
                    EnemyFleetCentre = enemyLocation
                    NumShips         = oldState.NumShips
                }

            | FleetEngagedOnMap(_, pauseStartTime, stateAfterPause) ->

                let elapsed = gameTime - pauseStartTime
                if elapsed > PauseTimeOnceEngaged then
                    { oldState with AlliedState = stateAfterPause }
                else
                    oldState

            | ScreenOverEngagedEnemyAtSea
            | ScreenOverEngagedSecretPassage ->
        
                oldState   // Ideology:  Never risk the logic rest of the logic when the screen is over.

    match newState.AlliedState with
        | MapInPlay(_)
        | FleetEngagedOnMap(_)           -> StayOnInitialMap(newState)
        | ScreenOverEngagedSecretPassage -> GoToSecretPassage(newState)
        | ScreenOverEngagedEnemyAtSea    -> GoToBattleAtSea(newState)

