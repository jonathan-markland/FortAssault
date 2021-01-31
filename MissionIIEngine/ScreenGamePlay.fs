module ScreenGamePlay

open Levels
open GamePlayDataModels
open GamePlayScreenConstants
open Rules
open GameStateManagement
open DrawingShapes
open DrawingFunctions
open ImagesAndFonts
open StaticResourceAccess
open Input
open Keys
open Sounds
open Time
open ScoreHiScore
open Geometry
open ResourceIDs
open Directions
open LevelTextToMatrix

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  DRAWING
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderMissionIIScreen render (model:ScreenModel) gameTime =

    let {
            InnerScreenModel     = innerScreenModel
            ScreenMan            = man
            ScreenDroids         = droids
            ScreenGhost          = ghost
            Bullets              = bullets
            DecorativeFlickbooks = decoratives
        } = model

    let {
            LevelNumber        = LevelNumber levelNumber
            LevelTileMatrix    = LevelTileMatrix levelTileMatrix
            RoomNumber         = RoomNumber roomNumber
            ScreenOriginPixel  = { ptx=levelPointX ; pty=levelPointY }
            ScreenOriginBlock  = (blockOriginX, blockOriginY)
            ScreenScore        = { Score=score ; HiScore = hiScore }
            ManInventory       = inventory
            ManLives           = ManLives lives
            Collectibles       = collectibles
            ImageLookupsTables = imageLookupTables
            WhereToOnGameOver  = _
        } = innerScreenModel

    let {
            BrickStyles       = brickStyles
            ManFacingStyles   = manFacingStyles
            ManWalkingStyles1 = manWalkingStyles1
            ManWalkingStyles2 = manWalkingStyles2
        } = imageLookupTables


    let drawBackground () =
        Rectangle render 0<epx> 0<epx> ScreenWidthInt ScreenHeightInt (SolidColour 0u)

    let drawTopLineOfScoreboard () =
        let scoreText = $"SCORE {score}"
        let roomText  = $"ROOM {roomNumber} L{levelNumber}"
        Text render MissionIIFontID LeftAlign  TopAlign TextIndent TopPanelTopY scoreText
        Text render MissionIIFontID RightAlign TopAlign (ScreenWidthInt - TextIndent) TopPanelTopY roomText

    let drawLives () =
        let lifeImage = LifeImageID |> ImageFromID
        let width = lifeImage.ImageMetadata.ImageWidth
        for i in 1u..lives do
            let x = (int (i-1u)) * width
            Image1to1 render x BottomPanelTopY lifeImage

    let inventoryItemImageFor item =
        (match item with
            | InvKey  -> KeyImageID
            | InvRing -> RingImageID
            | InvGold -> GoldImageID)
                |> ImageFromID

    let drawInventory () =
        inventory |> List.iteri (fun i item ->
            let x = ScreenWidthInt - ((int i) * InventoryItemSpacing)
            Image1to1 render x BottomPanelTopY (inventoryItemImageFor item))

    let drawTiles () =
        let brickImage  = WallBrick1ImageID |> ImageFromID
        let brickWidth  = brickImage.ImageMetadata.ImageWidth  // all are same size
        let brickHeight = brickImage.ImageMetadata.ImageHeight // all are same size
        for y in 0..NumBricksPerSide-1 do
            for x in 0..NumBricksPerSide-1 do
                let tile = levelTileMatrix.[blockOriginY+y].[blockOriginX+x]
                let x' = x * brickWidth + PlayAreaOffsetX
                let y' = y * brickHeight + PlayAreaOffsetY
                Image1to1 render x' y' (brickStyles.[int tile])

    let offset (point:ViewPoint) =
        let { ptx=x ; pty=y } = point
        { 
            ptx = ((float32 x) + (float32 PlayAreaOffsetX)) |> Float32ToEpx 
            pty = ((float32 y) + (float32 PlayAreaOffsetY)) |> Float32ToEpx 
        }

    let drawMan () =
        let { ManState=manState ; ManCentrePosition=manCentre } = man
        let manImage = 
            match manState with
                | ManStandingFacing direction -> manFacingStyles.[int direction]
                | ManWalking        direction -> (gameTime |> PulseBetween ManWalkingStepsPerSecond manWalkingStyles1 manWalkingStyles2).[int direction]
                | ManElectrocuted             ->  gameTime |> PulseBetween ManElectrocutionSwitchesPerSecond Electrocution1ImageID Electrocution2ImageID |> ImageFromID
                | ManDead                     -> DeadImageID |> ImageFromID
        CentreImagePoint render (manCentre |> offset) manImage

    drawBackground ()
    drawTopLineOfScoreboard ()
    drawLives ()
    drawInventory ()
    drawTiles ()
    // drawDroids ()
    drawMan ()


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Screen state advance on frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextMissionIIScreenState gameState keyStateGetter gameTime elapsed =

    (*

    // Unpack

    let model = ModelFrom gameState

    let {
        }
            = model

    // Process


    // Repack

    let model =
        {
        }        

    // Decide next gameState

    *)

    gameState |> Unchanged


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMissionIIScreen levelNumber whereToOnGameOver (betweenScreenStatus:BetweenScreenStatus) =

    // let numberOfMazes = AllLevels. levelIndex    = levelNumber % numberOfMazes

    let brickStyles =
        [|
            ImageFromID WallBrick1ImageID
            ImageFromID WallBrick2ImageID
            ImageFromID WallBrick3ImageID
            ImageFromID WallBrick4ImageID
            ImageFromID WallBrick5ImageID
            ImageFromID WallBrick6ImageID
        |]

    let manFacingStyles =
        [|
            ImageFromID FacingLeftImageID      
            ImageFromID FacingLeftUpImageID    
            ImageFromID FacingUpImageID        
            ImageFromID FacingRightUpImageID   
            ImageFromID FacingRightImageID     
            ImageFromID FacingRightDownImageID 
            ImageFromID FacingDownImageID      
            ImageFromID FacingLeftDownImageID  
        |]

    let manWalkingStyles1 =
        [|
            ImageFromID WalkingLeft1ImageID      
            ImageFromID WalkingLeftUp1ImageID    
            ImageFromID WalkingUp1ImageID        
            ImageFromID WalkingRightUp1ImageID   
            ImageFromID WalkingRight1ImageID     
            ImageFromID WalkingRightDown1ImageID 
            ImageFromID WalkingDown1ImageID      
            ImageFromID WalkingLeftDown1ImageID  
        |]

    let manWalkingStyles2 =
        [|
            ImageFromID WalkingLeft2ImageID      
            ImageFromID WalkingLeftUp2ImageID    
            ImageFromID WalkingUp2ImageID        
            ImageFromID WalkingRightUp2ImageID   
            ImageFromID WalkingRight2ImageID     
            ImageFromID WalkingRightDown2ImageID 
            ImageFromID WalkingDown2ImageID      
            ImageFromID WalkingLeftDown2ImageID  
        |]

    let screenModel =
        {
            InnerScreenModel =
                {
                    LevelNumber        = LevelNumber levelNumber
                    LevelTileMatrix    = AllLevels.[0] |> LevelTextToMatrix // TODO
                    RoomNumber         = RoomNumber 1
                    ScreenOriginPixel  = { ptx=0<LevelSpace> ; pty=0<LevelSpace> }
                    ScreenOriginBlock  = (0,0)
                    ScreenScore        = betweenScreenStatus.ScoreAndHiScore
                    ManInventory       = [ InvGold ; InvKey ; InvRing ] // TODO: remove
                    ManLives           = ManLives InitialLives
                    Collectibles       = [] // TODO
                    ImageLookupsTables =
                        {
                            BrickStyles       = brickStyles
                            ManFacingStyles   = manFacingStyles
                            ManWalkingStyles1 = manWalkingStyles1
                            ManWalkingStyles2 = manWalkingStyles2
                        }
                    WhereToOnGameOver  = whereToOnGameOver
                }

            ScreenMan =
                {
                    ManState = ManWalking Left8
                    ManCentrePosition = { ptx=160.0F<ViewSpace> ; pty=100.0F<ViewSpace> } // TODO
                }

            ScreenDroids =
                [
                    { DroidType = HomingDroid ; DroidCentrePosition = { ptx=100.0F<ViewSpace> ; pty=60.0F<ViewSpace> } } // TODO
                    { DroidType = HomingDroid ; DroidCentrePosition = { ptx=280.0F<ViewSpace> ; pty=110.0F<ViewSpace> } } // TODO
                ]

            ScreenGhost = NoGhost

            Bullets =
                [
                    { BulletCentrePosition = { ptx=120.0F<ViewSpace> ; pty=60.0F<ViewSpace> } }
                ]

            DecorativeFlickbooks = []
        }

    NewGameState NextMissionIIScreenState RenderMissionIIScreen screenModel



