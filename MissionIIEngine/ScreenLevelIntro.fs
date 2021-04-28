module ScreenLevelIntro

open GameStateManagement
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open Time
open StaticResourceAccess
open GamePlayScreenConstants
open Sounds

let ItemSeparation = 30<epx>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private LevelIntroScreenModel =
    {
        LevelMessage : string
        LevelIndex   : int
        FatFont      : Font
        BigFont      : Font
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderLevelIntroScreen render model (gameTime:float32<seconds>) =

    let backgroundImage = BackgroundImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    let x50pc = 50 |> PercentOfScreenWidth
    let y1 = 30 |> PercentOfScreenHeight
    let y2 = 55 |> PercentOfScreenHeight
    let y3 = 75 |> PercentOfScreenHeight

    TextX render model.BigFont CentreAlign MiddleAlign x50pc y1 model.LevelMessage
    TextX render model.FatFont CentreAlign MiddleAlign x50pc y2 "FIND  THE  FOLLOWING"

    // TODO: somehow unify with object placement needs
    let images = [KeyImageID ; RingImageID ; GoldImageID]
    let itemCountLessOne = min 2 model.LevelIndex
    let spanWidth = (itemCountLessOne * ItemSeparation)
    let leftPos = (ScreenWidthInt - spanWidth) / 2
    images |> List.iteri (fun i imageID -> 
        if i <= itemCountLessOne then
            let x = leftPos + (i * ItemSeparation)
            Image1to1 render x y3 (ImageFromID imageID))



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewLevelIntroScreen levelNumber =

    let smallFont = FontFromID MissionIIFontID

    let model =
        {
            LevelMessage = sprintf "Level %d" levelNumber
            LevelIndex   = (levelNumber - 1)
            FatFont      = MagnifiedFont  6  2 1  smallFont
            BigFont      = MagnifiedFont  6  4 6  smallFont
        }

    let soundCommands =
        [PlaySoundEffect (SoundFromID EnteringLevelSoundID)]

    NewGameStateAndSounds ModelNeverChanges RenderLevelIntroScreen model soundCommands


