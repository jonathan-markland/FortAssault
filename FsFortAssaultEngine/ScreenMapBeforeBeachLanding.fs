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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DefaultAlliedFleetLocation = { ptx=120.0F<epx> ; pty=97.0F<epx> }

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

type AlliedState =
    | FleetBeforeBeachLandingInPlay of PointF32
    | EngagedBeachLanding           of PointF32 * timeOfEngagement:float32<seconds>
    | ScreenOver

type MapBeforeBeachLandingScreenModel =
    {
        ScoreAndHiScore : ScoreAndHiScore
        ShipsThrough    : uint32
        AlliedState     : AlliedState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let AlliesVersusBeach alliesLocation gameTime =

    if alliesLocation |> IsPointWithinRectangle BeachLandingTriggerRectangle then
        EngagedBeachLanding(alliesLocation, gameTime)
    else
        FleetBeforeBeachLandingInPlay(alliesLocation)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderMapBeforeBeachLandingScreen render (model:MapBeforeBeachLandingScreenModel) =

    let imgMap = ImageMap |> ImageFromID

    Image1to1 render 0<epx> 0<epx> imgMap

    // PermissableTravelLocationRectangles |> List.iteri (fun i r ->
    //     render (DrawFilledRectangle(r.Left, r.Top, r |> RectangleWidth, r |> RectangleHeight, i |> AlternateOf 0xEE0000u 0x00FF00u)))

    match model.AlliedState with
        | FleetBeforeBeachLandingInPlay(location)
        | EngagedBeachLanding(location, _) ->
            CentreImage render location.ptx location.pty (ImageAlliedFleetSymbol |> ImageFromID)
        | ScreenOver ->
            ()

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

let NewMapBeforeBeachLandingScreen scoreAndHiScore shipsThrough =
    {
        ScoreAndHiScore  = scoreAndHiScore
        ShipsThrough     = shipsThrough
        AlliedState      = FleetBeforeBeachLandingInPlay(DefaultAlliedFleetLocation)
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextMapBeforeBeachLandingScreenState oldState keyStateGetter gameTime =

    let input = keyStateGetter |> DecodedInput

    match oldState.AlliedState with

        | FleetBeforeBeachLandingInPlay(alliedLocation) ->
    
            let alliedLocation = NewAlliedFleetLocation alliedLocation input PermissableTravelLocationRectangles
            let allies = AlliesVersusBeach alliedLocation gameTime

            {
                ScoreAndHiScore = oldState.ScoreAndHiScore
                ShipsThrough    = oldState.ShipsThrough
                AlliedState     = allies
            }

        | EngagedBeachLanding(_,engagementTime) ->

            let elapsedSinceEngagement = gameTime - engagementTime
            if elapsedSinceEngagement > PauseTimeOnceEngaged then
                { oldState with AlliedState = ScreenOver }
            else
                oldState

        | ScreenOver ->
        
            oldState   // Ideology:  Never risk the logic rest of the logic when the screen is over.


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Query functions for Storyboard
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let StayOnMapBeforeBeachLanding state =
    match state.AlliedState with
        | FleetBeforeBeachLandingInPlay _ -> true
        | EngagedBeachLanding _
        | ScreenOver                      -> false
