module ScreenIntermissions

open IntermissionCard
open FreezeFrame
open StaticResourceAccess
open ResourceIDs
open Rules


let WithLifeLossIntermissionCard whereToAfter gameTime =

    NewIntermissionCard
        (BackgroundImageID |> ImageFromID) 
        (GreyFontID |> FontFromID)
        "LIFE OVER    GET READY" 
        ScreenWidthInt ScreenHeightInt 
            |> WithFreezeFrameFor LifeOverGetReadyPauseTime gameTime whereToAfter
        
