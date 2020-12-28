module PacmanGetReadyOverlay

open Time
open DrawingFunctions
open ResourceIDs
open ScreenHandler
open ImagesAndFonts
open Sounds
open StaticResourceAccess



type PacmanGetReadyOverlayModel = unit



let private RenderGetReadyOverlay render (model:PacmanGetReadyOverlayModel) gameTime =
    let cx,cy = (ScreenWidthInt / 2) , (ScreenHeightInt / 3) 
    Text render GreyFontID CentreAlign  MiddleAlign cx cy "GET READY"



// The overlay provides a (possibly animated) message that
// an external party can draw over something else.
let NewPacmanGetReadyOverlay () =
    NewGameState ModelNeverChanges RenderGetReadyOverlay ()


// ----------------------------------------------------------------------------------------------------------
// TODO: move to library:


open FreezeFrame

type FreezeForGetReadyModel =
    {
        GameTimeLie     : float32<seconds>

        /// This is the ErasedGameState that is frozen in time, at time GameTimeLie.
        FrozenGameState : ErasedGameState

        /// The overlay that draws the GET READY message.
        Overlay         : ErasedGameState
    }


let RenderFreezeForGetReady render model (gameTime:float32<seconds>) =
    model.FrozenGameState.Draw render model.GameTimeLie
    model.Overlay.Draw render gameTime


let NextFreezeForGetReadyState gameState keyStateGetter gameTime elapsed =
    let model = ModelFrom gameState
    let nextOverlay = model.Overlay.Frame keyStateGetter gameTime elapsed
    let model = { model with Overlay = nextOverlay }
    gameState |> WithUpdatedModel model


let FreezeForGetReady newGame messageOverlay duration gameTime =

    let model =
        {
            GameTimeLie     = gameTime
            FrozenGameState = newGame gameTime  // Construction for the pause (will be thrown away)
            Overlay         = messageOverlay
        }

    let afterPauseFunc _outgoingState gameTime = 
        newGame gameTime  // Construction of the real game
            |> WithOneShotSound [PlaySoundEffect (SoundFromID GoSoundID)]

    NewGameState NextFreezeForGetReadyState RenderFreezeForGetReady model
        |> WithFreezeFrameFor duration gameTime afterPauseFunc
        |> WithOneShotSound [PlaySoundEffect (SoundFromID ThreeTwoOneSoundID)]
        

