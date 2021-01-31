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
open FlickBook

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
            RoomNumber         = roomNumber
            ScreenOriginBlock  = (blockOriginX, blockOriginY)
            ScreenScore        = { Score=score ; HiScore = hiScore }
            ManInventory       = inventory
            ManLives           = ManLives lives
            Interactible       = interactibles
            ImageLookupsTables = imageLookupTables
            WhereToOnGameOver  = _
        } = innerScreenModel

    let {
            BrickStyles       = brickStyles
            ManFacingStyles   = manFacingStyles
            ManWalkingStyles1 = manWalkingStyles1
            ManWalkingStyles2 = manWalkingStyles2
            DroidStyles1      = droidStyles1
            DroidStyles2      = droidStyles2
            InteractibleObjectStyles = interactibleObjectStyles
        } = imageLookupTables


    let drawBackground () =
        Rectangle render 0<epx> 0<epx> ScreenWidthInt ScreenHeightInt (SolidColour 0u)

    let drawTopLineOfScoreboard () =
        let (RoomNumber roomNumber) = roomNumber
        let scoreText = $"SCORE {score}"
        let roomText  = $"ROOM {roomNumber} L{levelNumber}"
        Text render MissionIIFontID LeftAlign  TopAlign TextIndent TopPanelTopY scoreText
        Text render MissionIIFontID RightAlign TopAlign (ScreenWidthInt - TextIndent) TopPanelTopY roomText

    let drawLives () =
        let lifeImage = LifeImageID |> ImageFromID
        let width = lifeImage.ImageMetadata.ImageWidth + LifeItemSpacing
        let n = min 10u lives
        for i in 1u..n do
            let x = (int i) * width
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

    let isEdge tile =
        match tile with
            | TileIndex.TileEdge1
            | TileIndex.TileEdge2 -> true
            | _ -> false

    let drawTiles () =
        let brickImage  = WallBrick1ImageID |> ImageFromID
        let brickWidth  = brickImage.ImageMetadata.ImageWidth  // all are same size
        let brickHeight = brickImage.ImageMetadata.ImageHeight // all are same size
        for y in 0..NumBricksPerSide-1 do
            for x in 0..NumBricksPerSide-1 do
                let tile = levelTileMatrix.[blockOriginY+y].[blockOriginX+x]
                let x' = x * brickWidth + PlayAreaOffsetX
                let y' = y * brickHeight + PlayAreaOffsetY
                let brick = 
                    if tile |> isEdge then
                        gameTime |> PulseBetween WallElectrocutionSwitchesPerSecond (brickStyles.[int tile]) (WallElectricImageID |> ImageFromID)
                    else
                        (brickStyles.[int tile])
                Image1to1 render x' y' brick

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

    let drawDroids () =
        droids |> List.iter (fun droid ->
            let {
                    DroidType           = droidType
                    DroidCentrePosition = centrePos
                } = droid

            let imageSet = gameTime |> PulseBetween DroidAnimationPerSecond droidStyles1 droidStyles2
            CentreImagePoint render (centrePos |> offset) imageSet.[int droidType]
        )

    let drawBullets () =
        bullets |> List.iter (fun bullet ->
            let { BulletCentrePosition = centrePos } = bullet
            CentreImagePoint render (centrePos |> offset) (BulletImageID |> ImageFromID)
        )

    let drawGhost () =
        match ghost with
            | NoGhost -> ()
            | GhostActive centrePos -> 
                CentreImagePoint render (centrePos |> offset) (GhostImageID |> ImageFromID)

    let drawInteractibles () =
        interactibles |> List.iter (fun interactible ->
            let {
                    InteractibleRoom           = interactibleRoomNumber
                    InteractibleType           = interactibleObjectType
                    InteractibleCentrePosition = centrePos
                } = interactible
            if roomNumber = interactibleRoomNumber then
                CentreImagePoint render (centrePos |> offset) interactibleObjectStyles.[int interactibleObjectType]
        )

    let drawDecoratives () =
        DrawFlickbookInstanceList render decoratives gameTime

    drawBackground ()
    drawTopLineOfScoreboard ()
    drawLives ()
    drawInventory ()
    drawTiles ()
    drawInteractibles ()
    drawBullets ()
    drawDroids ()
    drawMan ()
    drawGhost ()
    drawDecoratives ()



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Screen state advance on frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextMissionIIScreenState gameState keyStateGetter gameTime elapsed =
    gameState |> Unchanged


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMissionIIScreen levelNumber whereToOnGameOver (betweenScreenStatus:BetweenScreenStatus) _gameTime =

    // let numberOfMazes = AllLevels. levelIndex    = levelNumber % numberOfMazes

    let brickStyles =
        [|
            ImageFromID FloorTile1ImageID
            ImageFromID FloorTile2ImageID
            ImageFromID WallBrick3ImageID
            ImageFromID WallBrick4ImageID
            ImageFromID WallOutline1ImageID
            ImageFromID WallOutline2ImageID
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

    let droidStyles1 =
        [|
            ImageFromID Monster1v1ImageID
            ImageFromID Monster2v1ImageID
            ImageFromID Monster3ImageID  
            ImageFromID Monster4v1ImageID
            ImageFromID Monster5v1ImageID
        |]

    let droidStyles2 =
        [|
            ImageFromID Monster1v2ImageID
            ImageFromID Monster2v2ImageID
            ImageFromID Monster3ImageID  
            ImageFromID Monster4v2ImageID
            ImageFromID Monster5v2ImageID
        |]

    let interactibleObjectStyles =
        [|
            ImageFromID KeyImageID
            ImageFromID RingImageID
            ImageFromID GoldImageID
            ImageFromID InvincibilityAmuletImageID
            ImageFromID Potion1ImageID
            ImageFromID LevelExitImageID
        |]

    let screenModel =
        {
            InnerScreenModel =
                {
                    LevelNumber        = LevelNumber levelNumber
                    LevelTileMatrix    = AllLevels.[0] |> LevelTextToMatrix // TODO
                    RoomNumber         = RoomNumber 1
                    ScreenOriginBlock  = (0,0)
                    ScreenScore        = betweenScreenStatus.ScoreAndHiScore
                    ManInventory       = [ InvGold ; InvKey ; InvRing ] // TODO: remove
                    ManLives           = ManLives InitialLives
                    Interactible       = [] // TODO
                    ImageLookupsTables =
                        {
                            BrickStyles              = brickStyles
                            ManFacingStyles          = manFacingStyles
                            ManWalkingStyles1        = manWalkingStyles1
                            ManWalkingStyles2        = manWalkingStyles2
                            DroidStyles1             = droidStyles1
                            DroidStyles2             = droidStyles2
                            InteractibleObjectStyles = interactibleObjectStyles
                        }
                    WhereToOnGameOver  = whereToOnGameOver
                }

            ScreenMan =
                {
                    ManState = ManWalking EightWayDirection.Left8
                    ManCentrePosition = { ptx=160.0F<ViewSpace> ; pty=100.0F<ViewSpace> } // TODO
                }

            ScreenDroids =
                [
                    { DroidType = DroidType.HomingDroid    ; DroidCentrePosition = { ptx=100.0F<ViewSpace> ; pty= 60.0F<ViewSpace> } } // TODO
                    { DroidType = DroidType.WanderingDroid ; DroidCentrePosition = { ptx=280.0F<ViewSpace> ; pty=110.0F<ViewSpace> } } // TODO
                    { DroidType = DroidType.AssassinDroid  ; DroidCentrePosition = { ptx=230.0F<ViewSpace> ; pty= 80.0F<ViewSpace> } } // TODO
                ]

            ScreenGhost = NoGhost

            Bullets =
                [
                    { BulletCentrePosition = { ptx=120.0F<ViewSpace> ; pty=60.0F<ViewSpace> } }
                ]

            DecorativeFlickbooks = []
        }

    NewGameState NextMissionIIScreenState RenderMissionIIScreen screenModel



