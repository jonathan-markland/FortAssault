module MechanicsTestPage

open Time
open SharedDrawing
open DrawingCommands
open DrawingCommandsEx
open FontAlignment
open Geometry
open Mechanics
open ImagesAndFonts

let AnimDurationSeconds = 3.0F<seconds>
let AnimRepeatPeriod    = 5.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type MechanicsTestPageScreenModel =
    {
        Functions:    (float32<seconds> -> MOMReason) list
        RepeatAtTime:  float32<seconds>
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderMechanicsTestPageScreen render (model:MechanicsTestPageScreenModel) gameTime =

    Rectangle render 0<epx> 0<epx> ScreenWidthInt ScreenHeightInt (SolidColour(0x000000u))
    Text render YellowFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (15<epx>) "MECHANICS TEST SCREEN"
    Rectangle render 25<epx> 25<epx> 270<epx> 150<epx> (SolidColour(0x000050u))
    model.Functions |> List.iter (fun positionGetter ->
        match positionGetter gameTime with
            | MOMVisibleAtPosition( {ptx=x;pty=y} ) -> CentreImage render x y ImageAlliedFleetSymbol
            | _ -> ()
    )

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMechanicsTestPageScreen gameTime =

    let test motionFunction x =
        FunctionThatGetsPositionOfMovingObject 
            motionFunction 
            {ptx=x ; pty=150.0F<epx>}
            {ptx=x ; pty=50.0F<epx>}
            gameTime
            AnimDurationSeconds

    {
        RepeatAtTime = gameTime + AnimRepeatPeriod
        Functions = 
            [
                test LinearMotion        50.0F<epx>
                test SpeedingUpMotion   100.0F<epx>
                test SlowingDownMotion  150.0F<epx>
                test ArcMotion          200.0F<epx>
            ]
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextMechanicsTestPageScreenState oldState input gameTime =

    if gameTime > oldState.RepeatAtTime then
        NewMechanicsTestPageScreen gameTime
    else
        oldState
        