﻿module ScreenInitialMap

open Angle
open Time
open SharedDrawing
open DrawingFunctions
open ScoreHiScore
open Geometry
open ResourceIDs
open ScorePanel
open MapScreenSharedDetail
open Rules
open StaticResourceAccess
open InputEventData
open FreezeFrame
open ScreenHandler
open ScreenIntermission

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private PauseDuration = 2.0F<seconds>

let private DefaultAlliedFleetLocation = { ptx=290.0F<epx> ; pty=15.0F<epx> }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// These are permitted to overlap other rectangles, including the trigger rectangles.
let private PermissableTravelLocationRectangles =
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

type private InitialMapScreenModel =
    {
        ScoreAndHiScore     : ScoreAndHiScore
        AlliedFleetCentre   : PointF32
        EnemyFleetCentre    : PointF32
        SecretPassageCtor   : ScoreAndHiScore -> float32<seconds> -> ErasedGameState
        EngageEnemyCtor     : ScoreAndHiScore -> float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderInitialMapScreen render (model:InitialMapScreenModel) _gameTime =

    let imgMap = ImageMap |> ImageFromID

    Image1to1 render 0<epx> 0<epx> imgMap

    // PermissableTravelLocationRectangles |> List.iteri (fun i r ->
    //     render (DrawFilledRectangle(r.Left, r.Top, r |> RectangleWidth, r |> RectangleHeight, i |> AlternateOf 0xEE0000u 0x00FF00u)))

    CentreImage render model.AlliedFleetCentre.ptx model.AlliedFleetCentre.pty (ImageAlliedFleetSymbol |> ImageFromID)
    CentreImage render model.EnemyFleetCentre.ptx  model.EnemyFleetCentre.pty  (ImageEnemyFleetSymbol  |> ImageFromID)

    let mapHeight = imgMap.ImageMetadata.ImageHeight

    ScoreboardArea render mapHeight

    let scorePanel =
        {
            ScoreAndHiScore  = model.ScoreAndHiScore
            ShipsPending     = 0u
            ShipsThrough     = NumShipsAtInitialEngagement
            Tanks            = NumShipsAtInitialEngagement |> ToTankCountFromShipCount
            Damage           = 0u
            MaxDamage        = 0u
            PlaneIntel       = None
            Elevation        = 0.0F<degrees> // TODO:  Should this be optional?
        }

    DrawScorePanel render mapHeight scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextInitialMapScreenState gameState keyStateGetter gameTime elapsed =

    let input = keyStateGetter |> DecodedInput

    let model = ModelFrom gameState
    let alliedLocation = model.AlliedFleetCentre
    let enemyLocation  = model.EnemyFleetCentre

    let alliedLocation =
        NewAlliedFleetLocation alliedLocation input PermissableTravelLocationRectangles

    let enemyLocation =
        NewEnemyFleetLocation enemyLocation alliedLocation

    if alliedLocation |> IsPointWithinRectangle SecretPassageTriggerRectangle then
        
        let whereToAfter = 
            model.ScoreAndHiScore
                |> model.SecretPassageCtor 
                |> WithFortAssaultIntermissionCard

        gameState |> WithFreezeFrameFor PauseDuration gameTime whereToAfter

    elif alliedLocation |> IsWithinRegionOf enemyLocation EnemyEngagementDistance then
        
        let whereeToAfter = 
            model.ScoreAndHiScore
                |> model.EngageEnemyCtor 
                |> WithFortAssaultIntermissionCard
        
        gameState |> WithFreezeFrameFor PauseDuration gameTime whereeToAfter
    
    else
        gameState |> WithUpdatedModel
            {
                ScoreAndHiScore     = model.ScoreAndHiScore
                AlliedFleetCentre   = alliedLocation
                EnemyFleetCentre    = enemyLocation
                SecretPassageCtor   = model.SecretPassageCtor
                EngageEnemyCtor     = model.EngageEnemyCtor
            }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewInitialMapScreen secretPassageCtor engageEnemyCtor scoreAndHiScore =

    let mapModel =
        {
            ScoreAndHiScore   = scoreAndHiScore
            AlliedFleetCentre = DefaultAlliedFleetLocation
            EnemyFleetCentre  = DefaultEnemyFleetLocation
            SecretPassageCtor = secretPassageCtor
            EngageEnemyCtor   = engageEnemyCtor
        }

    NewGameState NextInitialMapScreenState RenderInitialMapScreen mapModel

