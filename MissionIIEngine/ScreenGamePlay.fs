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
open Algorithm


let GhostTriggerDistance        = 12.0F<ViewSpace>
let DroidTriggerDistance        = 12.0F<ViewSpace>
let BulletTriggerDistance       =  8.0F<ViewSpace>
let ManFiringStartDistance      = 10.0F
let InteractibleTriggerDistance = 10.0F<ViewSpace>



let IsCloseToAny things getThingCentre triggerDistance centre =
    things |> List.exists (fun thing -> thing |> getThingCentre |> IsWithinRegionOf centre triggerDistance)


let FireButtonJustPressed keyStateGetter =

    let {
            JustDown = justDown
            Held     = _
        } = (keyStateGetter (WebBrowserKeyCode 90)) // TODO: FIRE KEY CONSTANT!

    justDown



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  TRANSLATION
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let inline DimensionsToFloat32ViewSpace dims =  // TODO: possibly reconsider?
    {
        dimx = ((float32) dims.dimx) |> LanguagePrimitives.Float32WithMeasure<ViewSpace>
        dimy = ((float32) dims.dimy) |> LanguagePrimitives.Float32WithMeasure<ViewSpace>
    }

let offset (point:ViewPoint) =
    let { ptx=x ; pty=y } = point
    { 
        ptx = ((float32 x) + (float32 PlayAreaOffsetX)) |> Float32ToEpx 
        pty = ((float32 y) + (float32 PlayAreaOffsetY)) |> Float32ToEpx 
    }

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
            RoomNumber         = roomNumber
            RoomReference      = roomReference
            ScreenScore        = { Score=score ; HiScore = hiScore }
            ManInventory       = inventory
            ManLives           = ManLives lives
            Interactible       = interactibles
            ImageLookupsTables = imageLookupTables
            WhereToOnGameOver  = _
        } = innerScreenModel

    let {
            LevelTileMatrix   = LevelTileMatrix levelTileMatrix
            RoomOriginBrick   = (blockOriginX, blockOriginY)
        } = roomReference

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
            let x = ScreenWidthInt - ((int (i+1)) * InventoryItemSpacing)
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
//  Bullets
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewBulletFrom { ptx=x ; pty=y } startDistanceAway direction =

    let converted x = x |> float32 |> LanguagePrimitives.Float32WithMeasure<ViewSpace>
    let (dx,dy) = DeltasForEightWayDirection direction
    let (fdx,fdy) = (converted dx , converted dy)

    {
        BulletCentrePosition =
            {
                ptx = fdx * startDistanceAway
                pty = fdy * startDistanceAway
            }
    }


let AdvancedWithBulletsRemovedThatHitWallsOrOutsidePlayArea levelTileMatrix bullets =

        // Advance all bullets
        // Check against screen bounds
        // Check against walls

    bullets  // TODO



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Man
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let IsManAlive man =
    match man with
        | ManStandingFacing _ 
        | ManWalking        _ -> true
        | ManElectrocuted
        | ManDead             -> false

let ManExtents man =

    let {
            ManState          = state
            ManCentrePosition = centre
        } = man
        
    let manImageID =
        match state with
            | ManStandingFacing _
            | ManWalking        _ -> WalkingDown1ImageID
            | ManElectrocuted     -> Electrocution1ImageID
            | ManDead             -> DeadImageID

    let manImage = manImageID |> ImageFromID

    RectangleCenteredAbout centre (manImage |> ImageDimensionsF_v2 |> DimensionsToFloat32ViewSpace)


let RespondingToKeys keyStateGetter man =

        // If no direction keys pressed, leave current man state.
        // Else
        //    Calculate facing direction from keys.
        //    Calculate next proposed position from current pos and facing direction.
        //    (The man cannot be blocked by anything, he just risks electrocution).

    man  // TODO


let IntersectsRoomWallsOf roomReference manCentre =
    failwith "adfgsdfg" // TODO
    false

let IntersectsGhost ghost manCentre =
    match ghost with
        | NoGhost -> false
        | GhostActive ghostCentre ->
            manCentre |> IsWithinRegionOf ghostCentre GhostTriggerDistance

let IntersectsDroids droids manCentre =
    let getDroidCentre { DroidType=_ ; DroidCentrePosition=centre } = centre
    manCentre |> IsCloseToAny droids getDroidCentre DroidTriggerDistance

let IntersectsBullets bullets manCentre =
    let getBulletCentre { BulletCentrePosition=centre } = centre
    manCentre |> IsCloseToAny bullets getBulletCentre BulletTriggerDistance

let Electrocuted man =
    { man with ManState = ManElectrocuted }

let Dead man =
    { man with ManState = ManDead }

type InteractibleRemovalOption = KeepInteractible | RemoveInteractible
type InvincibilityTrigger = NoChangeInvincibility | GainInvincibility
type LivesDelta = NoExtraLife | ExtraLifeGained

let PossiblyInteractingWith currentRoomNumber interactibles man =

    // Reminder: Ignores level exit (filtered above).
    // We only need to interact with the first found.
    // Leave (theoretical) overlaps for the next frame.

    let manCentre = man.ManCentrePosition

    let touchedItem interactible =
        let {
                InteractibleRoom           = objectRoomNumber
                InteractibleType           = _
                InteractibleCentrePosition = objectCentre
            } = interactible

        objectRoomNumber = currentRoomNumber 
            && objectCentre |> IsWithinRegionOf manCentre InteractibleTriggerDistance

    let collect inventoryItem =
        (NoExtraLife, NoChangeInvincibility, Some inventoryItem)

    let interactionResultFor interactible =
        match interactible.InteractibleType with
            | InteractibleObjectType.ObKey         -> collect InvKey
            | InteractibleObjectType.ObRing        -> collect InvRing
            | InteractibleObjectType.ObGold        -> collect InvGold
            | InteractibleObjectType.ObAmulet      -> (NoExtraLife,     GainInvincibility,     None)
            | InteractibleObjectType.ObHealthBonus -> (ExtraLifeGained, NoChangeInvincibility, None)
            | InteractibleObjectType.ObLevelExit   -> (NoExtraLife,     NoChangeInvincibility, None)   // NB: No operation because of separate handling elsewhere.
            | _ -> failwith "Unrecognised enum case"

    let shouldBeRemoved interactible =
        interactible.InteractibleType <> InteractibleObjectType.ObLevelExit

    match interactibles |> List.tryFind touchedItem with
        | None -> (NoExtraLife, NoChangeInvincibility, None, interactibles)
        | Some interactible ->
            let (extraLife, invincib, invent) = interactionResultFor interactible
            if interactible |> shouldBeRemoved then
                (extraLife, invincib, invent, interactibles |> PlanetSavingListFilter (not << touchedItem))
            else
                (extraLife, invincib, invent, interactibles)

let PossiblyFiringAtDroids bullets keyStateGetter man = // bullets =
    
    if keyStateGetter |> FireButtonJustPressed then

        let { ManCentrePosition=centre ; ManState=state } = man

        match state with
            | ManStandingFacing direction 
            | ManWalking        direction -> (NewBulletFrom centre ManFiringStartDistance direction)::bullets
            | ManElectrocuted
            | ManDead -> bullets // no change

    else
        bullets // no change


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Droids
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let MovedBouncingAgainst (newManExtents, roomReference) gameTime droids = 
    // droids
    ()

let DroidsExplodedIfShotBy bullets droids = 
    // droids, decoratives =
    ()

let PossiblyFiringAtMan bullets gameTime droids =
    // bullets =
    ()


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Screen state advance on frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextMissionIIScreenState gameState keyStateGetter gameTime elapsed =

    let model = ModelFrom gameState

    let {
            InnerScreenModel     = innerScreenModel
            ScreenMan            = man
            ScreenDroids         = droids
            ScreenGhost          = ghost
            Bullets              = bullets
            DecorativeFlickbooks = decoratives
        } = model

    let {
            LevelNumber        = levelNumber
            RoomNumber         = roomNumber
            RoomReference      = roomReference
            ScreenScore        = { Score=score ; HiScore = hiScore }
            ManInventory       = inventory
            ManLives           = lives
            Interactible       = interactibles
            ImageLookupsTables = imageLookupTables
            WhereToOnGameOver  = _
        } = innerScreenModel

    let manOldExtents = man |> ManExtents

    let normalGamePlay () =

        // Man is alive.

        let bullets   = bullets |> AdvancedWithBulletsRemovedThatHitWallsOrOutsidePlayArea roomReference
        let man       = man |> RespondingToKeys keyStateGetter
        let manCentre = man.ManCentrePosition

        let manNewExtents = man |> ManExtents

        let man = 
            if (manCentre |> IntersectsRoomWallsOf roomReference) 
                || (manCentre |> IntersectsGhost ghost) 
                || (manCentre |> IntersectsDroids droids) then
                    Electrocuted man

            else if (manCentre |> IntersectsBullets bullets) then
                Dead man

            else
                man

        // TODO: We do not yet have data modelling for the invincibility.
        let livesDelta, invincibTrigger, newItemForInventory, interactibles = // TODO: Use below to generate next state
            man |> PossiblyInteractingWith roomNumber interactibles   // Reminder: Ignores level exit (filtered above).

        let droids =
            droids |> MovedBouncingAgainst (manNewExtents, roomReference) gameTime

        let droids, decoratives =
            droids |> DroidsExplodedIfShotBy bullets

        let bullets =
            man |> PossiblyFiringAtDroids bullets keyStateGetter

        let bullets =
            droids |> PossiblyFiringAtMan bullets gameTime

        gameState |> withNewStateApplied man lives inventory interactibles droids bullets decoratives


    let manAlive () =

        if manOldExtents |> intersectsLevelExit then
            failwith "Level exited"
        else if manOldExtents |> intersectsAnyRoomExit then
            failwith "room exited"
        else
            normalGamePlay ()


    match man.ManState with
        | ManStandingFacing _ 
        | ManWalking        _ -> manAlive ()
        | ManElectrocuted
        | ManDead             -> Unchanged gameState



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  New screen constructor
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
                    RoomNumber         = RoomNumber 1
                    RoomReference      =
                        {
                            LevelTileMatrix = AllLevels.[0] |> LevelTextToMatrix // TODO
                            RoomOriginBrick = (0,0)
                        }
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
                    ManCentrePosition = { ptx=220.0F<ViewSpace> ; pty=100.0F<ViewSpace> } // TODO
                }

            ScreenDroids =
                [
                    { DroidType = DroidType.HomingDroid    ; DroidCentrePosition = { ptx=100.0F<ViewSpace> ; pty= 60.0F<ViewSpace> } ; DroidDirection = EightWayDirection.Up8 } // TODO
                    { DroidType = DroidType.WanderingDroid ; DroidCentrePosition = { ptx=280.0F<ViewSpace> ; pty=110.0F<ViewSpace> } ; DroidDirection = EightWayDirection.Up8 } // TODO
                    { DroidType = DroidType.AssassinDroid  ; DroidCentrePosition = { ptx=230.0F<ViewSpace> ; pty= 80.0F<ViewSpace> } ; DroidDirection = EightWayDirection.Up8 } // TODO
                ]

            ScreenGhost = NoGhost

            Bullets =
                [
                    { BulletCentrePosition = { ptx=120.0F<ViewSpace> ; pty=60.0F<ViewSpace> } }
                ]

            DecorativeFlickbooks = []
        }

    NewGameState NextMissionIIScreenState RenderMissionIIScreen screenModel



