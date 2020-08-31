﻿module ScreenInitialMap

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


let DefaultAlliedFleetLocation = { xwf=290.0F<epx> ; ywf=15.0F<epx> }

/// These are permitted to overlap other rectangles, including the trigger rectangles.
let PermissableTravelLocationRectangles =
    [
        {
            Left   =  38.0F<epx>
            Top    =   7.0F<epx>
            Right  = 310.0F<epx>
            Bottom =  19.0F<epx>
        }
        {
            Left   = 125.0F<epx>
            Top    =  19.0F<epx>
            Right  = 310.0F<epx>
            Bottom =  35.0F<epx>
        }
        {
            Left   = 133.0F<epx>
            Top    =  35.0F<epx>
            Right  = 310.0F<epx>
            Bottom =  41.0F<epx>
        }
        {
            Left   = 168.0F<epx>
            Top    =  41.0F<epx>
            Right  = 310.0F<epx>
            Bottom =  50.0F<epx>
        }
        {
            Left   = 200.0F<epx>
            Top    =  49.0F<epx>
            Right  = 310.0F<epx>
            Bottom = 139.0F<epx>
        }
        {
            Left   =  86.0F<epx>
            Top    =  75.0F<epx>
            Right  = 276.0F<epx>
            Bottom = 139.0F<epx>
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

    Image1to1 render 0<epx> 0<epx> ImageMap.ImageID

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

    ScoreboardArea render (mapHeight |> FloatWuToIntEpx)

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

    DrawScorePanel render (mapHeight |> FloatWuToIntEpx) scorePanel

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

