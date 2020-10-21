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
open ScreenHandler
open FreezeFrame

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DefaultAlliedFleetLocation = { ptx=126.0F<epx> ; pty=76.0F<epx> }

/// These are permitted to overlap other rectangles, including the trigger rectangles.
let PermissableTravelLocationRectangles =
    [
        {
            Left   =  86.0F<epx>
            Top    =  75.0F<epx>
            Right  = 156.0F<epx>
            Bottom = 139.0F<epx>
        }
        BeachLandingTriggerRectangle
    ]

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type MapBeforeBeachLandingScreenModel =
    {
        ScoreAndHiScore  : ScoreAndHiScore
        ShipsThrough     : uint32
        Location         : PointF32
        BeachLandingCtor : float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderMapBeforeBeachLandingScreen render (model:MapBeforeBeachLandingScreenModel) _gameTime =

    let imgMap = ImageMap |> ImageFromID

    Image1to1 render 0<epx> 0<epx> imgMap

    // PermissableTravelLocationRectangles |> List.iteri (fun i r ->
    //     render (DrawFilledRectangle(r.Left, r.Top, r |> RectangleWidth, r |> RectangleHeight, i |> AlternateOf 0xEE0000u 0x00FF00u)))

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

let NextMapBeforeBeachLandingScreenState gameState keyStateGetter gameTime _elapsed =

    let input = keyStateGetter |> DecodedInput
    let model = ModelFrom gameState

    let alliedLocation = 
        NewAlliedFleetLocation model.Location input PermissableTravelLocationRectangles

    if alliedLocation |> IsPointWithinRectangle BeachLandingTriggerRectangle then
        gameState |> WithFreezeFrameFor PauseTimeOnceEngaged gameTime model.BeachLandingCtor

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

    let whereToAfter gameTime =
        whereToAfter scoreAndHiScore shipsThrough gameTime

    let mapModel =
        {
            ScoreAndHiScore  = scoreAndHiScore
            ShipsThrough     = shipsThrough
            Location         = DefaultAlliedFleetLocation
            BeachLandingCtor = whereToAfter
        }

    NewGameState NextMapBeforeBeachLandingScreenState RenderMapBeforeBeachLandingScreen mapModel
