module ScreenPacman

open Time
open DrawingFunctions
open ScoreHiScore
open Geometry
open ResourceIDs
open StaticResourceAccess
open FreezeFrame
open ScreenHandler
open ImagesAndFonts

// TODO: gameState |> WithFreezeFrameFor PauseDuration gameTime whereToAfter


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private PauseDuration = 2.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private PacmanScreenModel =
    {
        ScoreAndHiScore     : ScoreAndHiScore
        WhereToOnGameOver   : ScoreAndHiScore -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderPacmanScreen render (model:PacmanScreenModel) _gameTime =

    let backgroundImage = BackgroundImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextPacmanScreenState gameState keyStateGetter gameTime elapsed =

    // let input = keyStateGetter |> DecodedInput

    // let model = ModelFrom gameState
    // let alliedLocation = model.AlliedFleetCentre
    // let enemyLocation  = model.EnemyFleetCentre
    // 
    // let alliedLocation =
    //     NewAlliedFleetLocation alliedLocation input PermissableTravelLocationRectangles
    // 
    // let enemyLocation =
    //     NewEnemyFleetLocation enemyLocation alliedLocation
    // 
    // if alliedLocation |> IsPointWithinRectangle SecretPassageTriggerRectangle then
    //     
    //     let whereToAfter = 
    //         model.SecretPassageCtor |> WithFortAssaultIntermissionCard
    // 
    //     gameState |> WithFreezeFrameFor PauseDuration gameTime whereToAfter
    // 
    // elif alliedLocation |> IsWithinRegionOf enemyLocation EnemyEngagementDistance then
    //     
    //     let whereeToAfter = 
    //         model.EngageEnemyCtor |> WithFortAssaultIntermissionCard
    // 
    //     gameState |> WithFreezeFrameFor PauseDuration gameTime whereeToAfter
    // 
    // else
    //     gameState |> WithUpdatedModel
    //         {
    //             ScoreAndHiScore     = model.ScoreAndHiScore
    //             AlliedFleetCentre   = alliedLocation
    //             EnemyFleetCentre    = enemyLocation
    //             SecretPassageCtor   = model.SecretPassageCtor
    //             EngageEnemyCtor     = model.EngageEnemyCtor
    //         }

    Unchanged gameState

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanScreen whereToOnGameOver scoreAndHiScore =

    let pacModel =
        {
            ScoreAndHiScore   = scoreAndHiScore
            WhereToOnGameOver = whereToOnGameOver
        }

    NewGameState NextPacmanScreenState RenderPacmanScreen pacModel

