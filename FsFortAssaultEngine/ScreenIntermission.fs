module ScreenIntermission

open Time
open ResourceIDs
open StaticResourceAccess
open IntermissionCard


let WithFortAssaultIntermissionCard whereToAfter gameTime =

    WithIntermission
        4.0F<seconds>
        gameTime 
        (ImageIntermissionBackground |> ImageFromID) 
        (YellowFontID |> FontFromID)
        "GET READY" 
        ScreenWidthInt ScreenHeightInt 
        whereToAfter
        
