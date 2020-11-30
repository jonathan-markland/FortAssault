module ScreenIntermission

open Time
open ResourceIDs
open StaticResourceAccess
open IntermissionCard
open FreezeFrame


let WithFortAssaultIntermissionCard whereToAfter gameTime =

    NewIntermissionCard
        (ImageIntermissionBackground |> ImageFromID) 
        (YellowFontID |> FontFromID)
        "GET READY" 
        ScreenWidthInt ScreenHeightInt 
            |> WithFreezeFrameFor 4.0F<seconds> gameTime whereToAfter
        
