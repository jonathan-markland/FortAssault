module ScreenIntermissions

open IntermissionCard
open FreezeFrame
open StaticResourceAccess
open ResourceIDs
open Rules
open ScoreHiScore
        


let WithLifeLossIntermissionCard whereToAfter gameTime =

    NewIntermissionCard
        (BackgroundImageID |> ImageFromID) 
        (GreyFontID |> FontFromID)
        "LIFE OVER    GET READY" 
        ScreenWidthInt ScreenHeightInt 
            |> WithFreezeFrameFor LifeOverGetReadyPauseTime gameTime whereToAfter
        


let WithScreenCompleteIntermissionCard betweenScreenStatus whereToAfter gameTime =

    NewIntermissionCard
        (BackgroundImageID |> ImageFromID) 
        (GreyFontID |> FontFromID)
        (sprintf "SCREEN COMPLETE  SCORE %d" betweenScreenStatus.ScoreAndHiScore.Score)
        ScreenWidthInt ScreenHeightInt 
            |> WithFreezeFrameFor ScreenCompletePauseTime gameTime whereToAfter
        
