module ScreenMapBeforeBeachLanding

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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private DefaultAlliedFleetLocation = { ptx=135.0F<epx> ; pty=120.0F<epx> }

/// These are permitted to overlap other rectangles, including the trigger rectangles.
let private PermissableTravelLocationRectangles =
    [
        {
            Left   =  86.0F<epx>
            Top    =  75.0F<epx>
            Right  = 156.0F<epx>
            Bottom = 139.0F<epx>
        }
        {
            Left   =  68.0F<epx>
            Top    = 130.0F<epx>
            Right  =  91.0F<epx>
            Bottom = 141.0F<epx>
        }
        BeachLandingTriggerRectangle
    ]

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private MapBeforeBeachLandingScreenModel =
    {
        ScoreAndHiScore  : ScoreAndHiScore
        ShipsThrough     : uint32
        Location         : Point<float32<epx>>
        BeachLandingCtor : float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderMapBeforeBeachLandingScreen render (model:MapBeforeBeachLandingScreenModel) _gameTime =

    let imgMap = ImageMap |> ImageFromID
    Image1to1 render 0<epx> 0<epx> imgMap

    // DrawDebugRectangles render PermissableTravelLocationRectangles

    let location = model.Location
    CentreImage render location.ptx location.pty (ImageAlliedFleetSymbol |> ImageFromID)

    let mapHeight = imgMap.ImageMetadata.ImageHeight
    ScoreboardArea render mapHeight

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

    DrawScorePanel render mapHeight scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextMapBeforeBeachLandingScreenState gameState keyStateGetter gameTime _elapsed =

    let input = keyStateGetter |> DecodedInput
    let model = ModelFrom gameState

    let alliedLocation = 
        NewAlliedFleetLocation model.Location input PermissableTravelLocationRectangles

    if alliedLocation |> IsPointWithinRectangle BeachLandingTriggerRectangle then
        gameState |> WithFreezeFrameFor PauseTimeOnceEngaged gameTime (model.BeachLandingCtor |> AdaptedToIgnoreOutgoingStateParameter)

    else
        gameState |> WithUpdatedModel
            {
                ScoreAndHiScore  = model.ScoreAndHiScore
                ShipsThrough     = model.ShipsThrough
                Location         = alliedLocation
                BeachLandingCtor = model.BeachLandingCtor
            }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMapBeforeBeachLandingScreen scoreAndHiScore shipsThrough whereToAfter =

    let mapModel =
        {
            ScoreAndHiScore  = scoreAndHiScore
            ShipsThrough     = shipsThrough
            Location         = DefaultAlliedFleetLocation
            BeachLandingCtor = whereToAfter
        }

    NewGameState NextMapBeforeBeachLandingScreenState RenderMapBeforeBeachLandingScreen mapModel
