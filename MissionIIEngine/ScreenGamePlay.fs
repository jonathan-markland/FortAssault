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
open Collisions
open Mechanics
open ScoreHiScore


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  TODO:  FOR LIBRARY 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let EightWayDirectionApproximationFromTo (fromWhere:Point<float32<epx>>) (toWhere:Point<float32<epx>>) =

    let { ptx=x1 ; pty=y1 } = fromWhere
    let { ptx=x2 ; pty=y2 } = toWhere

    let   dx,dy   = (x2 - x1) , (y2 - y1)
    let  adx,ady  = abs dx , abs dy
    
    let twiceadx, twiceady = 2.0F * adx , 2.0F * ady

    if adx > twiceady then
        // Within 22.5 degrees above/below of the horizontal, to either side.
        if dx > 0.0F<epx> then EightWayDirection.Right8 else EightWayDirection.Left8

    else if ady > twiceadx then
        // Within 22.5 degrees left/right of the vertical, above or below.
        if dy > 0.0F<epx> then EightWayDirection.Down8 else EightWayDirection.Up8

    else if dx > 0.0F<epx> then
        // Right side of vertical, within 22.5 degrees of the 45 degree lines.
        if dy > 0.0F<epx> then EightWayDirection.DownRight8 else EightWayDirection.UpRight8

    else
        // Left side of vertical, within 22.5 degrees of the 45 degree lines.
        if dy > 0.0F<epx> then EightWayDirection.DownLeft8 else EightWayDirection.UpLeft8



let MovedBy8way movementDirection speed point =

    let (dx,dy) = DeltasForEightWayDirection movementDirection

    {
        ptx = point.ptx + ((dx |> IntToFloatEpx) * speed)
        pty = point.pty + ((dy |> IntToFloatEpx) * speed)
    }



let inline RotateClockwise8way (direction:EightWayDirection) =
    let newDirectionInt = (((int) direction) + 1) &&& 7
    LanguagePrimitives.EnumOfValue<int, EightWayDirection> (newDirectionInt)


let EightWayDirectionFromKeys left right up down =
    if left && up then Some EightWayDirection.UpLeft8
    else if left && down then Some EightWayDirection.DownLeft8
    else if right && up then Some EightWayDirection.UpRight8
    else if right && down then Some EightWayDirection.DownRight8
    else if left then Some EightWayDirection.Left8
    else if right then Some EightWayDirection.Right8
    else if up then Some EightWayDirection.Up8
    else if down then Some EightWayDirection.Down8
    else None



    





// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  PROPERTIES AND SMALL FUNCTIONS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DroidImageIndexFor droidType =
    match droidType with
        | HomingDroid      -> 0
        | WanderingDroid _ -> 1
        | AssassinDroid    -> 2

let BulletCentreOf { BulletCentrePosition = ViewPoint centre } = 
    centre

let VPBulletCentreOf { BulletCentrePosition = centre } = // TODO sort this out
    centre

let DroidCentreOf { DroidType=_ ; DroidCentrePosition=ViewPoint centre } =
    centre

let VPDroidCentreOf { DroidType=_ ; DroidCentrePosition=centre } =  // TODO: sort out this
    centre

let ManCentreOf { ManCentrePosition = ViewPoint centre } = 
    centre

let VPManCentreOf { ManCentrePosition=centre } =  // TODO: sort out this
    centre


let IsCloseToAny things getThingCentre triggerDistance (ViewPoint centre) =
    things |> List.exists (fun thing -> 
        let (ViewPoint thingCentre) = thing |> getThingCentre
        thingCentre |> IsWithinRegionOf centre triggerDistance)


    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  ROOMS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type RoomExitCases = ExitingLeft | ExitingRight | ExitingUp | ExitingDown | NotExitingRoom

let ManVersusExits man =

    let { ManState = state ; ManCentrePosition = position } = man

    let inRelationToExits (ViewPoint position) =
        
        let { ptx=x ; pty=y } = position

        if x < 0.0F<epx> then ExitingLeft
        else if y < 0.0F<epx> then ExitingUp
        else if x > RoomWidthPixels then ExitingRight
        else if y > RoomHeightPixels then ExitingDown
        else NotExitingRoom

    match state with
        | ManDead
        | ManElectrocuted -> NotExitingRoom
        | ManStandingFacing _
        | ManWalking _ -> position |> inRelationToExits

type NewRoomFlipData =
    {
        NewRoomNumber       : RoomNumber
        NewRoomManCentre    : ViewPoint
        NewRoomOrigin       : (int * int)
    }

let CheckForRoomFlip roomOrigin man =

    let (rx,ry) = roomOrigin

    let n = NumRoomsPerSide - 1

    let roomMoveDeltas =
        match man |> ManVersusExits with
            | ExitingLeft  -> if rx > 0 then Some (-1, 0) else failwith "Leftmost exit leads outside level bounds"
            | ExitingRight -> if rx < n then Some (+1, 0) else failwith "Rightmost exit leads outside level bounds"
            | ExitingUp    -> if ry > 0 then Some ( 0,-1) else failwith "Upward exit leads outside level bounds"
            | ExitingDown  -> if ry < n then Some ( 0,+1) else failwith "Downward exit leads outside level bounds"
            | NotExitingRoom -> None

    match roomMoveDeltas with
        | None -> None
        | Some (rdx,rdy) ->

            let mandx = ((float32) -rdx) * (RoomWidthPixels  - ManRoomFlipMargin)
            let mandy = ((float32) -rdy) * (RoomHeightPixels - ManRoomFlipMargin)

            let manCentre = ManCentreOf man

            let (rx,ry) = (rx+rdx, ry+rdy)

            Some 
                {
                    NewRoomNumber       = RoomNumber ((ry * NumRoomsPerSide) + rx + 1)
                    NewRoomManCentre    = ViewPoint { ptx=manCentre.ptx + mandx ; pty=manCentre.pty + mandy }
                    NewRoomOrigin       = (rx, ry)
                }

let RoomCornerFurthestFrom (ViewPoint point) =

    let rcx = RoomWidthPixels / 2.0F
    let rcy = RoomHeightPixels / 2.0F

    let cornerX = if point.ptx > rcx then 0.0F<epx> else RoomWidthPixels
    let cornerY = if point.pty > rcy then 0.0F<epx> else RoomHeightPixels

    ViewPoint { ptx=cornerX ; pty=cornerY }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  LEVEL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// The rule for the number of items required to exit the given numbered level.
let NumItemsRequiredOnLevel (LevelNumber levelNumber) =

    let itemCountNeededToExitLevel = min (max 1 levelNumber) 3
    itemCountNeededToExitLevel

/// Do we intersect the level exit, and do we have the required items?
let CheckForNextLevel currentLevelNumber (inventory:InventoryObjectType list) (interactibles:Interactible list) currentRoomNumber manCentre =

    let numCarrying = inventory.Length
    let numNeeded   = NumItemsRequiredOnLevel currentLevelNumber

    if numCarrying < numNeeded then
        false

    else
        interactibles |> List.exists (fun interactible ->
            
            let {
                    InteractibleRoom           = objectRoomNumber
                    InteractibleType           = objectType
                    InteractibleCentrePosition = ViewPoint objectCentre
                } = interactible

            objectType = InteractibleObjectType.ObLevelExit
                && objectRoomNumber = currentRoomNumber
                && objectCentre |> IsWithinRegionOf manCentre InteractibleTriggerDistance
        )
    



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  KEYBOARD
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let FireButtonJustPressed keyStateGetter =
    let { JustDown = justDown ; Held = _ } = keyStateGetter (WebBrowserKeyCode 90) // TODO: FIRE KEY CONSTANT!
    justDown

let LeftButtonHeld keyStateGetter =
    let { JustDown = _ ; Held = held } = keyStateGetter (WebBrowserKeyCode 37) // TODO
    held

let RightButtonHeld keyStateGetter =
    let { JustDown = _ ; Held = held } = keyStateGetter (WebBrowserKeyCode 39) // TODO
    held

let UpButtonHeld keyStateGetter =
    let { JustDown = _ ; Held = held } = keyStateGetter (WebBrowserKeyCode 38) // TODO
    held

let DownButtonHeld keyStateGetter =
    let { JustDown = _ ; Held = held } = keyStateGetter (WebBrowserKeyCode 40) // TODO
    held



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  TRANSLATION
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let inline DimensionsToFloat32Epx { dimx=dimx ; dimy=dimy } =  // TODO: possibly reconsider?
    {
        dimx = ((float32) dimx) |> Float32ToEpx
        dimy = ((float32) dimy) |> Float32ToEpx
    }

let offset point =
    let (ViewPoint { ptx=x ; pty=y }) = point
    { 
        ptx = ((float32 x) + (float32 PlayAreaOffsetX)) |> Float32ToEpx 
        pty = ((float32 y) + (float32 PlayAreaOffsetY)) |> Float32ToEpx 
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  FLICKBOOK TYPES
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ExplosionFlickBookType () =
    {
        FlickBookDuration       = ExplosionDuration
        FlickBookImages         = [| Explosion1ImageID |> ImageFromID ; Explosion2ImageID |> ImageFromID ; Explosion3ImageID |> ImageFromID |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Visible
    }

let NewExplosion centreLocation gameTime =
    {
        FlickBookType            = ExplosionFlickBookType ()
        FlickBookMechanicsObject = MechanicsControlledStationaryObject (centreLocation |> offset) gameTime ExplosionDuration
        FlickBookStartTime       = gameTime
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
            ManBullets           = manBullets
            DroidBullets         = droidBullets
            DecorativeFlickbooks = decoratives
        } = model

    let {
            RoomReference      = roomReference
            ScreenScore        = { Score=score ; HiScore = hiScore }
            ManInventory       = inventory
            ManLives           = ManLives lives
            Interactible       = interactibles
            ImageLookupsTables = imageLookupTables
            WhereToOnGameOver  = _
        } = innerScreenModel


    let {
            RoomNumber        = roomNumber
            RoomOrigin        = (roomOriginX, roomOriginY)
            LevelModel        = levelModel
        } = roomReference

    let {
            LevelNumber       = levelNumber
            LevelTileMatrix   = LevelTileMatrix levelTileMatrix
            TileMatrixTraits  = _
        } = levelModel

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
        let (LevelNumber levelNumber) = levelNumber
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
        let brickImage   = WallBrick1ImageID |> ImageFromID
        let brickWidth   = brickImage.ImageMetadata.ImageWidth  // all are same size
        let brickHeight  = brickImage.ImageMetadata.ImageHeight // all are same size
        let blockOriginX = roomOriginX * NumBricksPerSide
        let blockOriginY = roomOriginY * NumBricksPerSide
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
            CentreImagePoint render (centrePos |> offset) imageSet.[DroidImageIndexFor droidType]
        )

    let drawBullets bullets =
        bullets |> List.iter (fun bullet ->
            let { BulletCentrePosition = centrePos } = bullet
            CentreImagePoint render (centrePos |> offset) (BulletImageID |> ImageFromID)
        )

    let drawGhost () =
        match ghost with
            | NoGhostUntil _ -> ()
            | GhostActive centrePos -> 
                CentreImagePoint render (centrePos |> offset) (GhostImageID |> ImageFromID)
            | GhostStunned (centrePos,_) ->
                CentreImagePoint render (centrePos |> offset) (GhostStunnedImageID |> ImageFromID)

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
    drawBullets manBullets
    drawBullets droidBullets
    drawDroids ()
    drawMan ()
    drawGhost ()
    drawDecoratives ()


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  WALLS / BOUNDS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

open Tiles

let LevelTileMatrixDetails () =
    {
        TilesHorizontally = NumBricksPerSide * NumRoomsPerSide
        TilesVertically   = NumBricksPerSide * NumRoomsPerSide
        TileWidthPixels   = BrickTileWidth
        TileHeightPixels  = BrickTileHeight
    }

let IsWallTile tile =
    match tile with 
        | TileIndex.TileFloor1
        | TileIndex.TileFloor2 -> false
        | _ -> true

let IntersectsWalls hitTestDistance roomReference itemCentre =

    let hitTestDistance = hitTestDistance |> FloatEpxToIntEpx

    let (ViewPoint { ptx=x ; pty=y }) = itemCentre

    let xi = (x |> FloatEpxToIntEpx) - hitTestDistance
    let yi = (y |> FloatEpxToIntEpx) - hitTestDistance

    let viewportWindow =
        {
            WindowLeft   = xi
            WindowTop    = yi
            WindowWidth  = hitTestDistance * 2
            WindowHeight = hitTestDistance * 2
        }

    let (rx,ry) = roomReference.RoomOrigin
    let (ox,oy) = (rx * NumBricksPerSide , ry * NumBricksPerSide)

    let tilingOffset =  // from the top left of the viewport window
        {
            OffsetX = -(xi + (ox * BrickTileWidth) )
            OffsetY = -(yi + (oy * BrickTileHeight))
        }

    let mutable foundIntersection = false  // TODO: Provide variant of ForEachTileWithVisiblePortion in the library.

    ForEachTileWithVisiblePortion roomReference.LevelModel.TileMatrixTraits viewportWindow tilingOffset 
        (fun x y ix iy -> 
            let (LevelTileMatrix matrix) = roomReference.LevelModel.LevelTileMatrix
            let thisTile = matrix.[iy].[ix]
            foundIntersection <- foundIntersection || (thisTile |> IsWallTile)
        )

    foundIntersection

let OutOfPlayAreaBounds (ViewPoint point) =

    let sideh = (BrickTileWidth  * NumBricksPerSide) |> IntToFloatEpx
    let sidev = (BrickTileHeight * NumBricksPerSide) |> IntToFloatEpx

    point.ptx < 0.0F<epx> || point.pty < 0.0F<epx> || point.ptx > sideh || point.pty > sidev



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Bullets
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewBulletFrom (ViewPoint { ptx=x ; pty=y }) startDistanceAway direction =

    let converted x = x |> float32 |> Float32ToEpx
    let (dx,dy) = DeltasForEightWayDirection direction
    let (fdx,fdy) = (converted dx , converted dy)

    {
        BulletCentrePosition =
            ViewPoint
                {
                    ptx = x + fdx * startDistanceAway
                    pty = y + fdy * startDistanceAway
                }
        BulletDirection = direction
    }


let AdvancedWithBulletsRemovedThatHitWallsOrOutsidePlayArea roomReference bullets =

    let bulletMoved { BulletCentrePosition = ViewPoint centre ; BulletDirection = direction } = 
        { BulletCentrePosition = ViewPoint (centre |> MovedBy8way direction BulletSpeed) ; BulletDirection = direction }

    let whereBulletOutOfBounds { BulletCentrePosition = bulletPos } = 
        bulletPos |> IntersectsWalls BulletVsWallsTriggerDistance roomReference  ||  bulletPos |> OutOfPlayAreaBounds

    bullets |> List.map bulletMoved |> List.filter (not << whereBulletOutOfBounds)




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

    let { ManState = state ; ManCentrePosition = ViewPoint centre } = man
        
    let manImageID =
        match state with
            | ManStandingFacing _
            | ManWalking        _ -> WalkingDown1ImageID
            | ManElectrocuted     -> Electrocution1ImageID
            | ManDead             -> DeadImageID

    let manImage = manImageID |> ImageFromID

    RectangleCenteredAbout centre (manImage |> ImageDimensionsF_v2 |> DimensionsToFloat32Epx)

let RespondingToKeys keyStateGetter man =

    // The man cannot be blocked by anything, he just risks electrocution.

    let left  = keyStateGetter |> LeftButtonHeld
    let right = keyStateGetter |> RightButtonHeld
    let up    = keyStateGetter |> UpButtonHeld
    let down  = keyStateGetter |> DownButtonHeld

    if left || right || up || down then
        match EightWayDirectionFromKeys left right up down with
            | Some direction ->
                let newManPosition = (ManCentreOf man) |> MovedBy8way direction ManSpeed
                { ManState = ManWalking direction ; ManCentrePosition = ViewPoint newManPosition }
            | None ->
                failwith "Should never happen"
    else
        // No keys held this time.
        { 
            ManState = 
                match man.ManState with
                    | ManWalking direction -> ManStandingFacing direction
                    | _ -> man.ManState
            ManCentrePosition = man.ManCentrePosition
        }

let IntersectsRoomWallsOf roomReference manCentre =
    IntersectsWalls ManVsWallTriggerDistance roomReference manCentre

let IntersectsGhost ghost (ViewPoint manCentre) =
    match ghost with
        | NoGhostUntil _ -> false
        | GhostActive (ViewPoint ghostCentre) -> manCentre |> IsWithinRegionOf ghostCentre GhostTriggerDistance
        | GhostStunned _ -> false

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

let LivesIncrementedBy livesDelta manLives =
    match livesDelta with   
        | NoExtraLife -> manLives
        | ExtraLifeGained ->
            let (ManLives lives) = manLives
            ManLives (lives + 1u)

let PossiblyInteractingWith interactibles currentRoomNumber man =

    // Reminder: Ignores level exit (filtered above).
    // We only need to interact with the first found.
    // Leave (theoretical) overlaps for the next frame.

    let (ViewPoint manCentre) = man.ManCentrePosition

    let touchedItem interactible =
        let {
                InteractibleRoom           = objectRoomNumber
                InteractibleType           = _
                InteractibleCentrePosition = ViewPoint objectCentre
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

let PossiblyFiringAtDroids keyStateGetter man =
    
    if keyStateGetter |> FireButtonJustPressed then

        let { ManCentrePosition=centre ; ManState=state } = man

        match state with
            | ManStandingFacing direction 
            | ManWalking        direction -> Some (NewBulletFrom centre ManFiringStartDistance direction)
            | ManElectrocuted
            | ManDead -> None

    else
        None


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Droids
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let MovedToNewPositionsWhileConsidering (manCentre:ViewPoint) roomReference gameTime droids = 

    let intersectsWall droidCentre =
        IntersectsWalls DroidVsWallTriggerDistance roomReference droidCentre

    let intersectsMan (ViewPoint droidCentre) =
        let (ViewPoint manCentre) = manCentre
        droidCentre |> IsWithinRegionOf manCentre DroidVsManTriggerDistance

    let intersectsOtherDroids exceptThisDroidIndex (ViewPoint droidCentre) =
        let mutable flag = false  // TODO
        droids |> List.iteri (fun i otherDroid -> 
            if i <> exceptThisDroidIndex then
                flag <- droidCentre |> IsWithinRegionOf (DroidCentreOf otherDroid) DroidVsDroidTriggerDistance)
        flag

    let intersectsSomething droidIndex point =
        point |> intersectsWall 
            || point |> intersectsMan 
            || point |> intersectsOtherDroids droidIndex
            || point |> OutOfPlayAreaBounds

    let proposedLocationForHomingDroid centre =
        // (Without regard for intersections)
        let (ViewPoint centre) = centre
        let (ViewPoint manCentre) = manCentre
        let movementDirection = EightWayDirectionApproximationFromTo centre manCentre
        let newCentre = centre |> MovedBy8way movementDirection HomingDroidSpeed
        (ViewPoint newCentre, HomingDroid)

    let proposedLocationForWanderingDroid centre direction changeTime droidIndex gameTime =
        // (Without regard for intersections)
        let (ViewPoint centre) = centre

        let wanderingInDifferentDirection direction gameTime =
            // TODO: use gameTime as seed to choose
            let newDirection = direction |> RotateClockwise8way
            let nextDecisionTime = gameTime + WanderingDroidDecisionInterval
            WanderingDroid (newDirection, nextDecisionTime)

        if changeTime > gameTime then
            (ViewPoint centre, wanderingInDifferentDirection direction gameTime)
        else
            let newPosition = centre |> MovedBy8way direction WanderingDroidSpeed

            if (ViewPoint newPosition) |> intersectsSomething droidIndex then
                (ViewPoint centre, wanderingInDifferentDirection direction gameTime)
            else
                (ViewPoint newPosition, WanderingDroid (direction, changeTime))  // TODO: This is really returning the WanderingDroid unchanged.

    let proposedLocationForAssassinDroid centre =
        // (Without regard for intersections)
        let (ViewPoint centre) = centre
        let (ViewPoint manCentre) = manCentre
        let movementDirection = EightWayDirectionApproximationFromTo centre manCentre
        let newCentre = centre |> MovedBy8way movementDirection AssassinDroidSpeed
        (ViewPoint newCentre, AssassinDroid)

    let withBestEffortPositioning oldCentre droidIndex idealCentreWithoutRegardForOverlaps =

        let succeedsAt =
            not << (intersectsSomething droidIndex)

        let (ViewPoint { ptx=oldx ; pty=oldy }) = oldCentre
        let (ViewPoint { ptx=newx ; pty=newy }) = idealCentreWithoutRegardForOverlaps
        let verticallySlidPosition = ViewPoint { ptx=oldx ; pty=newy }

        if succeedsAt verticallySlidPosition then
            if succeedsAt idealCentreWithoutRegardForOverlaps then idealCentreWithoutRegardForOverlaps else verticallySlidPosition
        else
            let horizontallySlidPosition = ViewPoint { ptx=newx ; pty=oldy }
            if succeedsAt horizontallySlidPosition then horizontallySlidPosition else oldCentre

    let toNewDroidLocation droidIndex droid =
        
        let { DroidType=dtype ; DroidCentrePosition=oldCentre } = droid
        
        let idealCentreWithoutRegardForOverlaps , updatedDroidType =   // TODO: updatedDroidType not absolutely ideal just because we want wandering droid to change direction
            match dtype with
                | HomingDroid ->
                    proposedLocationForHomingDroid oldCentre

                | WanderingDroid (direction,changeTime) ->
                    proposedLocationForWanderingDroid oldCentre direction changeTime droidIndex gameTime

                | AssassinDroid ->
                    proposedLocationForAssassinDroid oldCentre
        
        let adjustedCentre =
            idealCentreWithoutRegardForOverlaps |> withBestEffortPositioning oldCentre droidIndex
        
        { DroidType=updatedDroidType ; DroidCentrePosition=adjustedCentre }

    droids |> List.mapi toNewDroidLocation



let DroidsExplodedIfShotBy bullets gameTime droids = 

    let doesBulletCollideWithDroid bullet droid =
        (BulletCentreOf bullet) |> IsWithinRegionOf (DroidCentreOf droid) BulletTriggerDistance

    let createExplosionAndScore bullet =   // TODO:  Explosion needs to be in the centre of the adversary, not the bullet centre.
        (NewExplosion (VPBulletCentreOf bullet) gameTime)  ,  ScoreForPlayerHittingDroid

    ResultOfProjectileCollisions
        bullets
        droids
        doesBulletCollideWithDroid
        BulletCentreOf  // used as identity
        DroidCentreOf   // used as identity
        createExplosionAndScore

let DroidsPossiblyFiring man (gameTime:float32<seconds>) droids =

    let possiblyFireIn direction (ViewPoint droidCentre) =
        let optBullet = Some (NewBulletFrom (ViewPoint droidCentre) DroidFiringStartDistance direction)
        gameTime |> PulseBetween 1.0F None optBullet  // TODO: Decide when droids fire really!

    let newBulletFiredByDroid droid =
        let droidCentre = DroidCentreOf droid
        match droid.DroidType with
            | HomingDroid -> 
                None  // never fires
            
            | WanderingDroid (direction,_) -> 
                possiblyFireIn direction (ViewPoint droidCentre)

            | AssassinDroid -> 
                let dir = EightWayDirectionApproximationFromTo droidCentre (ManCentreOf man)
                possiblyFireIn dir (ViewPoint droidCentre)

    droids |> List.choose newBulletFiredByDroid

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Ghost
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let GhostUpdatedWithRespectTo man manBullets gameTime ghost =

    let bulletThatIntersectsGhost ghostCentre bullet =
        (BulletCentreOf bullet) |> IsWithinRegionOf ghostCentre BulletTriggerDistance

    let shotBy manBullets (ViewPoint ghostCentre) =
        manBullets |> List.exists (bulletThatIntersectsGhost ghostCentre)

    match ghost with

        | NoGhostUntil appearanceTime ->
            if gameTime > appearanceTime then GhostActive (RoomCornerFurthestFrom (VPManCentreOf man)) else ghost

        | GhostActive ghostCentre -> 
            if ghostCentre |> shotBy manBullets then
                GhostStunned (ghostCentre,gameTime + GhostStunDuration)
            else
                let (ViewPoint ghostCentre) = ghostCentre
                let direction = EightWayDirectionApproximationFromTo ghostCentre (ManCentreOf man)
                let ghostCentre = ghostCentre |> MovedBy8way direction GhostSpeed
                GhostActive (ViewPoint ghostCentre)

        | GhostStunned (ghostCentre,reactivationTime) ->
            if ghostCentre |> shotBy manBullets then
                GhostStunned (ghostCentre,gameTime + GhostStunDuration)
            else if reactivationTime > gameTime then 
                GhostActive ghostCentre 
            else 
                ghost

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Apply level change
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Apply room flip
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let WithRoomFlipAppliedFrom roomFlipData gameTime model =

    let {
            NewRoomNumber       = newRoomNumber
            NewRoomManCentre    = newRoomManCentre
            NewRoomOrigin       = newRoomOrigin
        } = roomFlipData

    let model =
        {
            InnerScreenModel = 
                {
                    model.InnerScreenModel with
                        RoomReference =
                            {
                                RoomNumber = newRoomNumber
                                RoomOrigin = newRoomOrigin
                                LevelModel = model.InnerScreenModel.RoomReference.LevelModel
                            }
                }
            ScreenMan =
                {
                    ManState          = model.ScreenMan.ManState
                    ManCentrePosition = newRoomManCentre
                }
            ScreenDroids         = [] // TODO: new screen droids
            ScreenGhost          = NoGhostUntil (gameTime + GhostGraceDuration)
            ManBullets           = []
            DroidBullets         = []
            DecorativeFlickbooks = []
        }

    model
    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Screen state advance on frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private WithTheFollowingStateApplied
        gameTime
        man manBullets additionalManBullet
        droids droidBullets additionalDroidBullets
        ghost
        additionalExplosions1 additionalExplosions2
        additionalScore
        newItemForInventory livesDelta interactibles decoratives model =

    let innerScreenModel =  // TODO: possibly optimise further.
        {
            RoomReference      = model.InnerScreenModel.RoomReference // Will never change here because change handling done at higher level.
            ScreenScore        = model.InnerScreenModel.ScreenScore |> ScoreIncrementedBy additionalScore
            ManInventory       =
                match newItemForInventory with
                    | Some extra -> extra::model.InnerScreenModel.ManInventory
                    | None       -> model.InnerScreenModel.ManInventory
            ManLives           = model.InnerScreenModel.ManLives |> LivesIncrementedBy livesDelta
            Interactible       = interactibles
            ImageLookupsTables = model.InnerScreenModel.ImageLookupsTables
            WhereToOnGameOver  = model.InnerScreenModel.WhereToOnGameOver
        }

    let model =
        {
            InnerScreenModel  = innerScreenModel
            ScreenMan         = man
            ScreenDroids      = droids
            ScreenGhost       = ghost
            ManBullets        = 
                match additionalManBullet with
                    | Some extra -> extra::manBullets
                    | None       -> manBullets
            DroidBullets = 
                additionalDroidBullets 
                    |> List.append droidBullets
            DecorativeFlickbooks = 
                decoratives 
                    |> WithCompletedFlickbooksRemoved gameTime
                    |> List.append additionalExplosions1 
                    |> List.append additionalExplosions2
        }

    model

let private NextMissionIIScreenState gameState keyStateGetter gameTime elapsed =

    let model = ModelFrom gameState

    let {
            InnerScreenModel     = innerScreenModel
            ScreenMan            = man
            ScreenDroids         = droids
            ScreenGhost          = ghost
            ManBullets           = manBullets
            DroidBullets         = droidBullets
            DecorativeFlickbooks = decoratives
        } = model

    let {
            RoomReference      = roomReference
            ScreenScore        = { Score=score ; HiScore = hiScore }
            ManInventory       = inventory
            ManLives           = _
            Interactible       = interactibles
            ImageLookupsTables = _
            WhereToOnGameOver  = _
        } = innerScreenModel

    let {
            RoomNumber        = roomNumber
            RoomOrigin        = _
            LevelModel        = levelModel
        } = roomReference

    let {
            LevelNumber      = levelNumber
            LevelTileMatrix  = _
            TileMatrixTraits = _
        } = levelModel


    let normalGamePlay () =

        // Man is alive.

        let manBullets   = manBullets   |> AdvancedWithBulletsRemovedThatHitWallsOrOutsidePlayArea roomReference
        let droidBullets = droidBullets |> AdvancedWithBulletsRemovedThatHitWallsOrOutsidePlayArea roomReference
        
        let man       = man |> RespondingToKeys keyStateGetter
        let manCentre = man.ManCentrePosition

        let man = 
            if (manCentre |> IntersectsRoomWallsOf roomReference) 
                || (manCentre |> IntersectsGhost ghost) 
                || (manCentre |> IntersectsDroids droids) then
                    Electrocuted man

            else if (manCentre |> IntersectsBullets droidBullets) then
                Dead man

            else
                man

        // TODO: We do not yet have data modelling for the invincibility.
        let livesDelta, invincibTrigger, newItemForInventory, interactibles = // TODO: Use below to generate next state
            man |> PossiblyInteractingWith interactibles roomNumber   // Reminder: Ignores level exit (filtered above).

        let droids =
            droids |> MovedToNewPositionsWhileConsidering (VPManCentreOf man) roomReference gameTime

        let manBullets, droids, additionalExplosions1, additionalScore =
            droids |> DroidsExplodedIfShotBy manBullets gameTime

        let droidBullets, droids, additionalExplosions2, _ =
            droids |> DroidsExplodedIfShotBy droidBullets gameTime

        let additionalManBullet =
            man |> PossiblyFiringAtDroids keyStateGetter

        let additionalDroidBullets =
            droids |> DroidsPossiblyFiring man gameTime

        let ghost =
            ghost |> GhostUpdatedWithRespectTo man manBullets gameTime

        model |> WithTheFollowingStateApplied
            gameTime
            man manBullets additionalManBullet
            droids droidBullets additionalDroidBullets
            ghost
            additionalExplosions1 additionalExplosions2
            additionalScore
            newItemForInventory livesDelta interactibles decoratives


    let manAlive () =

        let manCentre = ManCentreOf man

        let model =
            match manCentre |> CheckForNextLevel levelNumber inventory interactibles roomNumber with
                | true ->
                    failwith "TODO: next level"
                    // model |> WithLevelChangeApplied
                | false ->
                    match CheckForRoomFlip roomReference.RoomOrigin man with
                        | Some roomFlipData ->
                            model |> WithRoomFlipAppliedFrom roomFlipData gameTime
                        | None ->
                            normalGamePlay ()

        gameState |> WithUpdatedModel model


    match man.ManState with
        | ManStandingFacing _ 
        | ManWalking        _ -> manAlive ()
        | ManElectrocuted
        | ManDead             -> Unchanged gameState






// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  New screen constructor
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMissionIIScreen levelNumber whereToOnGameOver (betweenScreenStatus:BetweenScreenStatus) gameTime =

    // TODO: sort out   let numberOfMazes = AllLevels. levelIndex    = levelNumber % numberOfMazes

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
                    RoomReference =
                        {
                            RoomNumber       = RoomNumber 1
                            RoomOrigin       = (0,0)
                            LevelModel       =
                                {
                                    LevelNumber      = LevelNumber levelNumber
                                    LevelTileMatrix  = AllLevels.[0] |> LevelTextToMatrix // TODO
                                    TileMatrixTraits = LevelTileMatrixDetails ()
                                }
                        }
                    ScreenScore        = betweenScreenStatus.ScoreAndHiScore
                    ManInventory       = []
                    ManLives           = ManLives InitialLives
                    Interactible       = 
                        [
                            {
                                InteractibleRoom           = RoomNumber 2
                                InteractibleType           = InteractibleObjectType.ObKey
                                InteractibleCentrePosition = ViewPoint { ptx=200.0F<epx> ; pty=100.0F<epx> }
                            }
                            {
                                InteractibleRoom           = RoomNumber 4
                                InteractibleType           = InteractibleObjectType.ObRing
                                InteractibleCentrePosition = ViewPoint { ptx=200.0F<epx> ; pty=100.0F<epx> }
                            }
                            {
                                InteractibleRoom           = RoomNumber 8
                                InteractibleType           = InteractibleObjectType.ObHealthBonus
                                InteractibleCentrePosition = ViewPoint { ptx=100.0F<epx> ; pty=100.0F<epx> }
                            }
                            {
                                InteractibleRoom           = RoomNumber 8
                                InteractibleType           = InteractibleObjectType.ObLevelExit
                                InteractibleCentrePosition = ViewPoint { ptx=200.0F<epx> ; pty=120.0F<epx> }
                            }
                            {
                                InteractibleRoom           = RoomNumber 6
                                InteractibleType           = InteractibleObjectType.ObLevelExit
                                InteractibleCentrePosition = ViewPoint { ptx=200.0F<epx> ; pty=80.0F<epx> }
                            }
                            {
                                InteractibleRoom           = RoomNumber 5
                                InteractibleType           = InteractibleObjectType.ObGold
                                InteractibleCentrePosition = ViewPoint { ptx=100.0F<epx> ; pty=100.0F<epx> }
                            }
                        ]
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
                    WhereToOnGameOver = whereToOnGameOver
                }

            ScreenMan =
                {
                    ManState          = ManWalking EightWayDirection.Left8
                    ManCentrePosition = ViewPoint { ptx=220.0F<epx> ; pty=100.0F<epx> } // TODO
                }

            ScreenDroids =
                [
                    // { DroidType = HomingDroid ; DroidCentrePosition = ViewPoint { ptx=100.0F<epx> ; pty= 60.0F<epx> } } // TODO
                    // { DroidType = HomingDroid ; DroidCentrePosition = ViewPoint { ptx=80.0F<epx> ; pty= 90.0F<epx> } } // TODO
                    // { DroidType = WanderingDroid (EightWayDirection.Up8, _gameTime) ; DroidCentrePosition = ViewPoint { ptx=280.0F<epx> ; pty=110.0F<epx> } } // TODO
                    // { DroidType = AssassinDroid ; DroidCentrePosition = ViewPoint { ptx=90.0F<epx> ; pty= 80.0F<epx> } } // TODO
                ]

            ScreenGhost = NoGhostUntil (gameTime + GhostGraceDuration)

            ManBullets =
                [
                    { BulletCentrePosition = ViewPoint { ptx=120.0F<epx> ; pty=60.0F<epx> } ; BulletDirection = EightWayDirection.DownRight8 }
                ]

            DroidBullets =
                [
                    { BulletCentrePosition = ViewPoint { ptx=60.0F<epx> ; pty=160.0F<epx> } ; BulletDirection = EightWayDirection.Up8 }
                ]

            DecorativeFlickbooks = []
        }

    NewGameState NextMissionIIScreenState RenderMissionIIScreen screenModel



