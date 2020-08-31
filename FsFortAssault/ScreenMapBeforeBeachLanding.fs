module ScreenMapBeforeBeachLanding

open Angle
open Time
open Rules
open SharedDrawing
open DrawingCommandsEx
open ScoreHiScore
open Geometry
open ImagesAndFonts
open ScorePanel
open MapScreenSharedDetail
open StoryboardChapters


let DefaultAlliedFleetLocation = { xwf=120.0F<wu> ; ywf=97.0F<wu> }

/// These are permitted to overlap other rectangles, including the trigger rectangles.
let PermissableTravelLocationRectangles =
    [
        {
            Left   =  86.0F<wu>
            Top    =  75.0F<wu>
            Right  = 156.0F<wu>
            Bottom = 139.0F<wu>
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

    Image1to1 render 0<wu> 0<wu> ImageMap.ImageID

    // PermissableTravelLocationRectangles |> List.iteri (fun i r ->
    //     render (DrawFilledRectangle(r.Left, r.Top, r |> RectangleWidth, r |> RectangleHeight, i |> AlternateOf 0xEE0000u 0x00FF00u)))

    match model.AlliedState with
        | FleetBeforeBeachLandingInPlay(location)
        | EngagedBeachLanding(location, _) ->
            CentreImage render location.xwf location.ywf ImageAlliedFleetSymbol
        | ScreenOver ->
            ()

    let mapHeight = ImageMap.ImageHeight

    ScoreboardArea render (mapHeight |> FloatWuToIntWu)

    let scorePanel =
        {
            ScoreAndHiScore  = model.ScoreAndHiScore
            ShipsPending     = 0u
            ShipsThrough     = model.ShipsThrough
            Tanks            = model.ShipsThrough |> ToTankCountFromShipCount
            Damage           = 0u
            Ammunition       = 10u
            Elevation        = 0.0F<degrees>
        }

    DrawScorePanel render (mapHeight  |> FloatWuToIntWu)scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMapBeforeBeachLandingScreen scoreAndHiScore shipsThrough =
    {
        ScoreAndHiScore  = scoreAndHiScore
        ShipsThrough     = shipsThrough
        AlliedState      = FleetBeforeBeachLandingInPlay(DefaultAlliedFleetLocation)
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextMapBeforeBeachLandingScreenState oldState input gameTime =

    let newModel =
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

    match newModel.AlliedState with

        | FleetBeforeBeachLandingInPlay(_) ->
            StayOnThisChapter1(newModel)

        | _ ->
            GoToNextChapter1(newModel)

