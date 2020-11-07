module ScreenIntermissions

open Time
open ResourceIDs
open StaticResourceAccess
open IntermissionCard
open FreezeFrame


let WithLifeLossIntermissionCard whereToAfter gameTime =

    NewIntermissionCard
        (BackgroundImageID |> ImageFromID) 
        (GreyFontID |> FontFromID)
        "LIFE OVER" 
        ScreenWidthInt ScreenHeightInt 
            |> WithFreezeFrameFor 4.0F<seconds> gameTime whereToAfter
        
