module ScreenIntermissions

open IntermissionCard
open FreezeFrame
open StaticResourceAccess
open ResourceIDs
open Rules
open ScoreHiScore
open GamePlayScreenConstants



let WithLifeLossIntermissionCard whereToAfter gameTime =

    NewIntermissionCard
        (BackgroundImageID |> ImageFromID) 
        (MissionIIFontID |> FontFromID)
        "LIFE OVER" 
        ScreenWidthInt ScreenHeightInt 
            |> WithFreezeFrameFor LifeOverGetReadyPauseTime gameTime (whereToAfter |> AdaptedToIgnoreOutgoingStateParameter)
        


let WithScreenCompleteIntermissionCard betweenScreenStatus whereToAfter gameTime =

    NewIntermissionCard
        (BackgroundImageID |> ImageFromID) 
        (MissionIIFontID |> FontFromID)
        (sprintf "SCREEN COMPLETE  SCORE %d" betweenScreenStatus.ScoreAndHiScore.Score)
        ScreenWidthInt ScreenHeightInt 
            |> WithFreezeFrameFor ScreenCompleteStatusCardTime gameTime (whereToAfter |> AdaptedToIgnoreOutgoingStateParameter)
        
