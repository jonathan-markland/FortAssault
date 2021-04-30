module ScreenMapPostPassage

open Angle
open Time
open Rules
open SharedDrawing
open DrawingFunctions
open ScoreHiScore
open Geometry
open ResourceIDs
open ScorePanel
open MapScreenSharedDetail
open ImagesAndFonts
open StaticResourceAccess
open InputEventData
open GameStateManagement
open FreezeFrame


let private DefaultAlliedFleetLocation = { ptx=124.0F<epx> ; pty=75.0F<epx> }

/// These are permitted to overlap other rectangles, including the trigger rectangles.
let private PermissableTravelLocationRectangles =
    [
        {
            Left   =  86.0F<epx>
            Top    =  75.0F<epx>
            Right  = 276.0F<epx>
            Bottom = 139.0F<epx>
        }
    ]

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private MapPostPassageScreenModel =
    {
        ScoreAndHiScore   : ScoreAndHiScore
        ShipsThrough      : uint32
        AlliedFleetCentre : Point<float32<epx>>
        EnemyFleetCentre  : Point<float32<epx>>
        BattleCtor        : GameTime -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderMapPostPassageScreen render (model:MapPostPassageScreenModel) _gameTime =

    let imgMap = (ImageMap |> ImageFromID)

    Image1to1 render 0<epx> 0<epx> imgMap

    // DrawDebugRectangles render PermissableTravelLocationRectangles

    let location = model.AlliedFleetCentre
    CentreImage render location.ptx location.pty (ImageAlliedFleetSymbol |> ImageFromID)
    CentreImage render model.EnemyFleetCentre.ptx model.EnemyFleetCentre.pty (ImageEnemyFleetSymbol |> ImageFromID)

    let h = imgMap.ImageMetadata.ImageHeight

    ScoreboardArea render h

    let scorePanel =
        {
            ScoreAndHiScore  = model.ScoreAndHiScore
            ShipsPending     = 0u
            ShipsThrough     = model.ShipsThrough
            Tanks            = model.ShipsThrough |> ToTankCountFromShipCount
            Damage           = 0u
            MaxDamage        = 0u
            PlaneIntel       = None
            Elevation        = 0.0F<degrees>
        }

    DrawScorePanel render h scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextMapPostPassageScreenState gameState keyStateGetter gameTime _elapsed =

    let input = keyStateGetter |> DecodedInput
    let model = ModelFrom gameState

    let alliedLocation = NewAlliedFleetLocation model.AlliedFleetCentre input PermissableTravelLocationRectangles
    let enemyLocation  = NewEnemyFleetLocation model.EnemyFleetCentre alliedLocation

    if alliedLocation |> IsWithinRegionOf enemyLocation EnemyEngagementDistance then
        gameState |> WithFreezeFrameFor PauseTimeOnceEngaged gameTime (model.BattleCtor |> AdaptedToIgnoreOutgoingStateParameter)

    else
        gameState |> WithUpdatedModel
            {
                ScoreAndHiScore   = model.ScoreAndHiScore
                ShipsThrough      = model.ShipsThrough
                AlliedFleetCentre = alliedLocation
                EnemyFleetCentre  = enemyLocation
                BattleCtor        = model.BattleCtor
            }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMapPostPassageScreen scoreAndHiScore shipsThrough whereToAfter =

    let mapModel =
        {
            ScoreAndHiScore   = scoreAndHiScore
            ShipsThrough      = shipsThrough
            AlliedFleetCentre = DefaultAlliedFleetLocation
            EnemyFleetCentre  = DefaultEnemyFleetLocation
            BattleCtor        = whereToAfter
        }

    NewGameState NextMapPostPassageScreenState RenderMapPostPassageScreen mapModel

