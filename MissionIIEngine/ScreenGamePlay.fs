module ScreenGamePlay

open Algorithm
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
open Random
open SustainModeUntil


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  TODO:  FOR LIBRARY 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewLocationForAttractor oldLocation attractionPoint travelSpeed =  // TODO: use in fort assault map screen too

    if oldLocation |> IsWithinRegionOf attractionPoint 1.0F<epx> then
        oldLocation
    else
        let delta = oldLocation |> SimpleMovementDeltaToGetTo attractionPoint travelSpeed
        oldLocation |> PointMovedByDelta delta


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



let inline RotateClockwise8way stepsDelta (fromDirection:EightWayDirection) =
    let newDirectionInt = (((int) fromDirection) + stepsDelta) &&& 7
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

let ManExclusionRectangleAround (ViewPoint manCentre) =
    let manImage = ImageFromID FacingUpImageID
    let w = manImage.ImageMetadata.ImageWidth
    let h = manImage.ImageMetadata.ImageHeight
    RectangleCenteredAbout { ptx=(manCentre.ptx) |> FloatEpxToIntEpx ; pty=(manCentre.pty) |> FloatEpxToIntEpx } { dimx=w ; dimy=h } 2

let DroidImageIndexFor droidType =
    match droidType with
        | HomingDroid      -> 0
        | WanderingDroid _ -> 1
        | AssassinDroid    -> 2

let BulletCentreOf { BulletCentrePosition = ViewPoint centre } = 
    centre

let VPBulletCentreOf { BulletCentrePosition = centre } = // TODO sort this out
    centre

let ToDroidIdentity (i:int) =
    DroidIdentity ((uint32) i)

let DroidIdentity { DroidIdentity=id ; DroidType=_ ; DroidCentrePosition=_ } =
    id

let DroidCentreOf { DroidIdentity=_ ; DroidType=_ ; DroidCentrePosition=ViewPoint centre } =
    centre

let VPDroidCentreOf { DroidIdentity=_ ; DroidType=_ ; DroidCentrePosition=centre } =  // TODO: sort out this
    centre

let ManCentreOf { ManCentrePosition = ViewPoint centre } = 
    centre

let VPManCentreOf { ManCentrePosition=centre } =  // TODO: sort out this
    centre

let CanDroidTypeFire { DroidIdentity=_ ; DroidType=droidType ; DroidCentrePosition=_ } =
    match droidType with
        | HomingDroid      -> false
        | WanderingDroid _ -> true
        | AssassinDroid    -> true

let IsCloseToAny things getThingCentre triggerDistance (ViewPoint centre) =
    things |> List.exists (fun thing -> 
        let (ViewPoint thingCentre) = thing |> getThingCentre
        thingCentre |> IsWithinRegionOf centre triggerDistance)


    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  ROOMS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RoomNumberFromRoomOrigin roomOrigin =
    let (RoomOrigin (rx,ry)) = roomOrigin
    RoomNumber ((ry * NumRoomsPerSide) + rx + 1)

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
        NewRoomOrigin    : RoomOrigin
        NewRoomManCentre : ViewPoint
    }

let CheckForRoomFlip roomOrigin man =

    let (RoomOrigin (rx,ry)) = roomOrigin

    let roomMoveDeltas =

        let n = NumRoomsPerSide - 1

        match man |> ManVersusExits with
            | ExitingLeft  -> if rx > 0 then Some (-1, 0) else failwith "Leftmost exit leads outside level bounds"
            | ExitingRight -> if rx < n then Some (+1, 0) else failwith "Rightmost exit leads outside level bounds"
            | ExitingUp    -> if ry > 0 then Some ( 0,-1) else failwith "Upward exit leads outside level bounds"
            | ExitingDown  -> if ry < n then Some ( 0,+1) else failwith "Downward exit leads outside level bounds"
            | NotExitingRoom -> None

    match roomMoveDeltas with
        | None -> None
        | Some (rdx,rdy) ->

            let manRoomFlipMoveDelta =
                {
                    modx = ((float32) -rdx) * (RoomWidthPixels  - ManRoomFlipMargin)
                    mody = ((float32) -rdy) * (RoomHeightPixels - ManRoomFlipMargin)
                }

            Some {
                    NewRoomOrigin    = RoomOrigin (rx+rdx, ry+rdy)
                    NewRoomManCentre = ViewPoint (ManCentreOf man |> PointMovedByDelta manRoomFlipMoveDelta)
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
let NumItemsRequiredOnLevel (LevelIndex levelNumber) =

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
    let { JustDown = justDown ; Held = _ } = keyStateGetter (WebBrowserKeyCode 90) // TODO: Constants, see also the inhibitor function below.
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

let Offset point =
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
        FlickBookMechanicsObject = MechanicsControlledStationaryObject (centreLocation |> Offset) gameTime ExplosionDuration
        FlickBookStartTime       = gameTime
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  OBJECT POSITION FINDER
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    
open Tiles

let IsWallTile tile =
    match tile with 
        | TileIndex.TileFloor1
        | TileIndex.TileFloor2 -> false
        | _ -> true

/// Yields a sequence of the centre positions of the available spare-space squares.
let AvailableObjectPositionsWithinRoom roomReference exclusionRectangles (desiredSquareSide:int<epx>) =

    let (RoomOrigin (rx,ry)) = roomReference.RoomOrigin
    let (ox,oy) = (rx * NumBricksPerSide * BrickTileWidth , ry * NumBricksPerSide * BrickTileHeight)

    let halfSide = desiredSquareSide / 2

    seq {

        let mutable ty = 0<epx>
        while ty < RoomHeightPixelsInt do

            let mutable tx = 0<epx>
            while tx < RoomWidthPixelsInt do
                
                let viewportWindow =
                    {
                        WindowLeft   = tx
                        WindowTop    = ty
                        WindowWidth  = desiredSquareSide
                        WindowHeight = desiredSquareSide
                    }

                let tilingOffset =  // from the top left of the viewport window
                    {
                        OffsetX = -(tx + ox)
                        OffsetY = -(ty + oy)
                    }

                let mutable intersectsWall = false  // TODO: Provide variant of ForEachTileWithVisiblePortion in the library.

                ForEachTileWithVisiblePortion roomReference.LevelModel.TileMatrixTraits viewportWindow tilingOffset 
                    (fun x y ix iy -> 
                        let (LevelTileMatrix matrix) = roomReference.LevelModel.LevelTileMatrix
                        let thisTile = matrix.[iy].[ix]
                        intersectsWall <- intersectsWall || (thisTile |> IsWallTile)
                    )

                if not intersectsWall then
                    let areaRect = SquareWithTopLeftAt { ptx=tx ; pty=ty } desiredSquareSide
                    let areaIntersectsAnyOfTheExclusionRectangles =
                        exclusionRectangles |> List.exists (RectangleIntersects areaRect)
                    if not areaIntersectsAnyOfTheExclusionRectangles then
                        let centreSpot = (tx + halfSide,ty + halfSide)
                        yield centreSpot

                tx <- tx + desiredSquareSide

            ty <- ty + desiredSquareSide
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
            RoomOrigin        = roomOrigin
            LevelModel        = levelModel
        } = roomReference

    let {
            LevelIndex       = levelIndex
            LevelTileMatrix   = levelTileMatrix
            TileMatrixTraits  = _
        } = levelModel

    let {
            BrickStyles       = brickStyles
            ManFacingStyles   = manFacingStyles
            ManWalkingStyles1 = manWalkingStyles1
            ManWalkingStyles2 = manWalkingStyles2
            DroidStyles1      = droidStyles1
            DroidStyles2      = droidStyles2
            InteractibleObjectImages = interactibleObjectImages
        } = imageLookupTables

    let roomNumber = roomOrigin |> RoomNumberFromRoomOrigin
    let (RoomOrigin (roomOriginX,roomOriginY)) = roomOrigin

    let drawBackground () =
        Rectangle render 0<epx> 0<epx> ScreenWidthInt ScreenHeightInt (SolidColour 0u)

    let drawTopLineOfScoreboard () =
        let fatFont = MagnifiedFont  6  2 1  (FontFromID MissionIIFontID)
        let (RoomNumber roomNumber) = roomNumber
        let scoreText = $"SCORE {score}"
        let (LevelIndex levelIndex) = levelIndex
        let roomText  = $"ROOM {roomNumber} L{levelIndex + 1}"
        TextX render fatFont LeftAlign  TopAlign TextIndent TopPanelTopY scoreText
        TextX render fatFont RightAlign TopAlign (ScreenWidthInt - TextIndent) TopPanelTopY roomText

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
        let (LevelTileMatrix levelTileMatrix) = levelTileMatrix
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
        CentreImagePoint render (manCentre |> Offset) manImage

    let drawDroids () =
        droids |> List.iter (fun droid ->
            let {
                    DroidType           = droidType
                    DroidCentrePosition = centrePos
                } = droid

            let imageSet = gameTime |> PulseBetween DroidAnimationPerSecond droidStyles1 droidStyles2
            CentreImagePoint render (centrePos |> Offset) imageSet.[DroidImageIndexFor droidType]
        )

    let drawBullets bullets =
        bullets |> List.iter (fun bullet ->
            let { BulletCentrePosition = centrePos } = bullet
            CentreImagePoint render (centrePos |> Offset) (BulletImageID |> ImageFromID)
        )

    let drawGhost () =
        match ghost with
            | NoGhostUntil _ -> ()
            | GhostActive centrePos -> 
                CentreImagePoint render (centrePos |> Offset) (GhostImageID |> ImageFromID)
            | GhostStunned (centrePos,_) ->
                CentreImagePoint render (centrePos |> Offset) (GhostStunnedImageID |> ImageFromID)

    let drawInteractibles () =
        interactibles |> List.iter (fun interactible ->
            let {
                    InteractibleRoom           = interactibleRoomNumber
                    InteractibleType           = interactibleObjectType
                    InteractibleCentrePosition = centrePos
                } = interactible
            if roomNumber = interactibleRoomNumber then
                let image = interactibleObjectImages.[InteractibleImageIndexFor interactibleObjectType]
                CentreImagePoint render (centrePos |> Offset) image
        )

    let drawDecoratives () =
        DrawFlickbookInstanceList render decoratives gameTime

    // DEBUG:
    let drawPotentialObjectPositionsInRoom () =
        
        let side = LargestAdversaryDimension

        let potentialObjectPositionsInRoom = 
            AvailableObjectPositionsWithinRoom roomReference [] side

        let colour = gameTime |> PulseBetween 10.0F (SolidColour 0xFF0000u) (SolidColour 0x0000FFu) 

        potentialObjectPositionsInRoom
            |> Seq.iter (fun (x,y) -> 
                Rectangle render (x+PlayAreaOffsetX) (y+PlayAreaOffsetY) (2<epx>) (2<epx>) colour)

    let drawManExclusionDebugRectangle () =
        let r = ManExclusionRectangleAround (VPManCentreOf man)
        Rectangle render (r.Left+PlayAreaOffsetX) (r.Top+PlayAreaOffsetY) (r |> RectangleWidth) (r |> RectangleHeight) (SolidColour 0x000040u)


    drawBackground ()
    drawTopLineOfScoreboard ()
    drawLives ()
    drawInventory ()
    drawTiles ()
    drawInteractibles ()
    drawBullets manBullets
    drawBullets droidBullets
    drawDroids ()
    // FOR DEBUG:  drawManExclusionDebugRectangle ()
    drawMan ()
    drawGhost ()
    drawDecoratives ()
    // FOR DEBUG:  drawPotentialObjectPositionsInRoom ()

    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  WALLS / BOUNDS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let LevelTileMatrixDetails () =
    {
        TilesHorizontally = NumBricksPerSide * NumRoomsPerSide
        TilesVertically   = NumBricksPerSide * NumRoomsPerSide
        TileWidthPixels   = BrickTileWidth
        TileHeightPixels  = BrickTileHeight
    }

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

    let (RoomOrigin (rx,ry)) = roomReference.RoomOrigin
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

    RectangleCenteredAbout centre (manImage |> ImageDimensionsF_v2 |> DimensionsToFloat32Epx) 2.0F

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

    // We only need to interact with the first found, because it will
    // be removed on this frame, and we can leave any (theoretical) 
    // overlapping interactible items for the next frame.

    let (ViewPoint manCentre) = man.ManCentrePosition

    let isItemTouched interactible =
        let {
                InteractibleRoom           = objectRoomNumber
                InteractibleType           = _
                InteractibleCentrePosition = ViewPoint objectCentre
            } = interactible

        objectRoomNumber = currentRoomNumber 
            && objectCentre |> IsWithinRegionOf manCentre InteractibleTriggerDistance

    let interactionResultFor interactibleType =

        let collect inventoryItem =
            (NoExtraLife, NoChangeInvincibility, Some inventoryItem)

        match interactibleType with
            | ObKey         -> collect InvKey
            | ObRing        -> collect InvRing
            | ObGold        -> collect InvGold
            | ObAmulet      -> (NoExtraLife,     GainInvincibility,     None)
            | ObHealthBonus -> (ExtraLifeGained, NoChangeInvincibility, None)
            | ObLevelExit   -> (NoExtraLife,     NoChangeInvincibility, None)   // NB: No operation because of separate handling elsewhere.

    let possibleFirstTouchedInteractible = 
        interactibles |> List.tryFind isItemTouched

    match possibleFirstTouchedInteractible with
        
        | None -> 
            (NoExtraLife, NoChangeInvincibility, None, interactibles)
        
        | Some interactible ->

            let shouldBeRemoved interactible =
                interactible.InteractibleType <> InteractibleObjectType.ObLevelExit

            let (livesDelta, invincibilityTrigger, itemGained) = 
                interactionResultFor interactible.InteractibleType

            let interactibles = 
                if interactible |> shouldBeRemoved then
                    interactibles |> PlanetSavingListFilter (not << isItemTouched)
                else
                    interactibles

            (livesDelta, invincibilityTrigger, itemGained, interactibles)

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

let NRandomChosenThings numRequired (things:'t[]) randomSeed =

    if numRequired > things.Length then
        failwith "Array isn't large enough to supply requested number of randomly chosen items"

    let (sequence, randomState) = ShuffledArrayAsSeq things randomSeed
    (sequence |> Seq.take numRequired, randomState)



type DroidTypeToMake = MakeHoming | MakeWandering | MakeAssassin      

let NewDroidsForRoom levelNumber placesForAdversariesInThisRoom gameTime =
   
    let numberToMake = 10

    let randomSeed = XorShift32State (uint32 gameTime)
    let (chosenPositions,_) = NRandomChosenThings numberToMake placesForAdversariesInThisRoom randomSeed

    let newDroidTypeToMakeFor (LevelIndex levelIndex) i =  // TODO: Throughout this should be LevelIndex if it is going to be zero-based!
        if levelIndex = 0 then
            MakeHoming
        else if levelIndex = 1 then
            if i < 3 then MakeWandering else MakeHoming
        else
            if i < 3 then 
                MakeAssassin 
            else if i < 5 then
                MakeWandering
            else
                MakeHoming

    let newDroidBy levelNumber i newDroidCentre =

        let (droidX,droidY) = newDroidCentre
        let centre   = ViewPoint { ptx=droidX ; pty=droidY } 
        let identity = i |> ToDroidIdentity

        match newDroidTypeToMakeFor levelNumber i with
        
            | MakeHoming -> 
                { 
                    DroidIdentity       = identity
                    DroidType           = HomingDroid
                    DroidCentrePosition = centre
                }

            | MakeWandering ->
                { 
                    DroidIdentity       = identity
                    DroidType           = WanderingDroid (EightWayDirection.Up8, gameTime)
                    DroidCentrePosition = centre
                }

            | MakeAssassin ->
                { 
                    DroidIdentity       = identity
                    DroidType           = AssassinDroid
                    DroidCentrePosition = centre
                }


    chosenPositions
        |> Seq.mapi (fun i (centreX,centreY) ->
            let newDroidCentre = (centreX |> IntToFloatEpx, centreY |> IntToFloatEpx)
            newDroidBy levelNumber i newDroidCentre)
        |> Seq.toList



let MovedToNewPositionsWhileConsidering (manCentre:ViewPoint) roomReference gameTime droids = 

    let intersectsWall droidCentre =
        IntersectsWalls DroidVsWallTriggerDistance roomReference droidCentre

    let intersectsMan (ViewPoint droidCentre) =
        let (ViewPoint manCentre) = manCentre
        droidCentre |> IsWithinRegionOf manCentre DroidVsManTriggerDistance

    let intersectsDroidsIn droidList (ViewPoint droidCentre) =
        not
            (droidList |> List.forall (fun otherDroid -> 
                not (droidCentre |> IsWithinRegionOf (DroidCentreOf otherDroid) DroidVsDroidTriggerDistance)))

    let intersectsSomethingIncluding otherDroidsNotYetMoved droidsMovedSoFar point =
        point |> intersectsWall 
            || point |> intersectsMan 
            || point |> intersectsDroidsIn otherDroidsNotYetMoved
            || point |> intersectsDroidsIn droidsMovedSoFar
            || point |> OutOfPlayAreaBounds

    let proposedLocationForHomingDroid centre =
        // (Without regard for intersections)
        let (ViewPoint centre) = centre
        let (ViewPoint manCentre) = manCentre
        let newCentre = NewLocationForAttractor centre manCentre HomingDroidSpeed
        (ViewPoint newCentre, HomingDroid)

    let proposedLocationForWanderingDroid centre direction changeTime otherDroidsNotYetMoved droidsMovedSoFar gameTime =

        // (Without regard for intersections)

        // Note: the droids may "stick" a bit because they are beside a wall, and are
        //       turning through an angle and only move once they turn towards a free direction.

        let (ViewPoint centre) = centre

        let rotationAmountByTime (time:float32<seconds>) =
            let delta = (((int) (time * 7.0F)) % 5) - 2  // 7.0 is arbitrary.
            System.Diagnostics.Debug.Assert (delta >= -2 && delta <= 2)
            delta

        let wanderingInDifferentDirection oldDirection gameTime =
            let newDirection = oldDirection |> RotateClockwise8way (rotationAmountByTime gameTime)
            let nextDecisionTime = gameTime + WanderingDroidDecisionInterval
            WanderingDroid (newDirection, nextDecisionTime)

        if gameTime > changeTime then
            (ViewPoint centre, wanderingInDifferentDirection direction gameTime)
        else
            let newPosition = centre |> MovedBy8way direction WanderingDroidSpeed

            if (ViewPoint newPosition) |> intersectsSomethingIncluding otherDroidsNotYetMoved droidsMovedSoFar then
                (ViewPoint centre, wanderingInDifferentDirection direction gameTime)
            else
                (ViewPoint newPosition, WanderingDroid (direction, changeTime))  // TODO: This is really returning the WanderingDroid unchanged.

    let proposedLocationForAssassinDroid centre =
        // (Without regard for intersections)
        let (ViewPoint centre) = centre
        let (ViewPoint manCentre) = manCentre
        let newCentre = NewLocationForAttractor centre manCentre AssassinDroidSpeed
        (ViewPoint newCentre, AssassinDroid)

    let withBestEffortPositioning oldCentre otherDroidsNotYetMoved droidsMovedSoFar idealCentreWithoutRegardForOverlaps =

        let succeedsAt =
            not << (intersectsSomethingIncluding otherDroidsNotYetMoved droidsMovedSoFar)

        let (ViewPoint { ptx=oldx ; pty=oldy }) = oldCentre
        let (ViewPoint { ptx=newx ; pty=newy }) = idealCentreWithoutRegardForOverlaps
        let verticallySlidPosition = ViewPoint { ptx=oldx ; pty=newy }

        if succeedsAt verticallySlidPosition then
            if succeedsAt idealCentreWithoutRegardForOverlaps then idealCentreWithoutRegardForOverlaps else verticallySlidPosition
        else
            let horizontallySlidPosition = ViewPoint { ptx=newx ; pty=oldy }
            if succeedsAt horizontallySlidPosition then horizontallySlidPosition else oldCentre

    let toNewDroidLocation droid otherDroidsNotYetMoved droidsMovedSoFar =
        
        let { DroidIdentity=id ; DroidType=dtype ; DroidCentrePosition=oldCentre } = droid
        
        let idealCentreWithoutRegardForOverlaps , updatedDroidType =   // TODO: updatedDroidType not absolutely ideal just because we want wandering droid to change direction
            match dtype with
                | HomingDroid ->
                    proposedLocationForHomingDroid oldCentre

                | WanderingDroid (direction,changeTime) ->
                    proposedLocationForWanderingDroid oldCentre direction changeTime otherDroidsNotYetMoved droidsMovedSoFar gameTime

                | AssassinDroid ->
                    proposedLocationForAssassinDroid oldCentre
        
        let adjustedCentre =
            idealCentreWithoutRegardForOverlaps |> withBestEffortPositioning oldCentre otherDroidsNotYetMoved droidsMovedSoFar
        
        { DroidIdentity=id ; DroidType=updatedDroidType ; DroidCentrePosition=adjustedCentre }

    droids |> UpgradedListMap toNewDroidLocation



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
        DroidIdentity
        createExplosionAndScore



let DroidsPossiblyFiring man (gameTime:float32<seconds>) droids =

    let possiblyFireIn direction (ViewPoint droidCentre) =
        Some (NewBulletFrom (ViewPoint droidCentre) DroidFiringStartDistance direction)

    let towards (ViewPoint dest) (ViewPoint source) =
        EightWayDirectionApproximationFromTo source dest

    let newBulletFiredByDroid droid =
        let droidCentre = VPDroidCentreOf droid
        match droid.DroidType with
            | HomingDroid -> 
                None  // never fires
            
            | WanderingDroid (direction,_) -> 
                possiblyFireIn direction droidCentre

            | AssassinDroid -> 
                let direction = droidCentre |> towards (VPManCentreOf man)
                possiblyFireIn direction droidCentre

    let filteredForDroidsThatCanFire droidList =
        droidList |> List.filter CanDroidTypeFire

    let shouldConsiderNow =
        ((int) (gameTime * 50.0F)) % 50 = 0   // TODO: Hack until I refactor use of float32<seconds> throughout in favour of integer 'FrameCount of uint32'?

    if shouldConsiderNow then
        match droids |> filteredForDroidsThatCanFire with
            | [] -> []
            | droids ->
                let randomSeed = XorShift32State (uint32 gameTime)
                let (chosenDroids,_) = NRandomChosenThings 1 (droids |> List.toArray) randomSeed
                chosenDroids |> Seq.choose newBulletFiredByDroid |> Seq.toList // TODO: Use seq in interface?
    else
        []



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
                let ghostCentre = NewLocationForAttractor ghostCentre (ManCentreOf man) GhostSpeed
                GhostActive (ViewPoint ghostCentre)

        | GhostStunned (ghostCentre,reactivationTime) ->
            if ghostCentre |> shotBy manBullets then
                GhostStunned (ghostCentre,gameTime + GhostStunDuration)
            else if gameTime > reactivationTime then 
                GhostActive ghostCentre 
            else 
                ghost


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Style resources
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let StyleResources levelIndex =

    let moduloImageChoice count (ImageID firstImageIndex) offset =
        let i = firstImageIndex + (offset % count)
        ImageID i

    let brickStyles =
        [|
            ImageFromID (moduloImageChoice NumFloorTileImages FloorTile1ImageID levelIndex)
            ImageFromID (moduloImageChoice NumFloorTileImages FloorTile1ImageID (levelIndex + 1))
            ImageFromID (moduloImageChoice NumWallTileImages WallBrick1ImageID levelIndex)
            ImageFromID (moduloImageChoice NumWallTileImages WallBrick1ImageID (levelIndex + 1))
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

    {
        BrickStyles              = brickStyles
        ManFacingStyles          = manFacingStyles
        ManWalkingStyles1        = manWalkingStyles1
        ManWalkingStyles2        = manWalkingStyles2
        DroidStyles1             = droidStyles1
        DroidStyles2             = droidStyles2
        InteractibleObjectImages = interactibleObjectStyles
    }



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Start level
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ModelForStartingLevel levelIndex whereToOnGameOver (betweenScreenStatus:BetweenScreenStatus) gameTime =

    let numberOfMazes = AllLevels.Length
    let levelIndex    = levelIndex % numberOfMazes
    let levelMatrix   = AllLevels.[levelIndex] |> LevelTextToMatrix

    // TODO:  let sparePlacesForInteractibles = PlacesWhereObjectsCanBeLocatedInLevel levelMatrix |> Seq.toArray

    let roomReference =
        {
            RoomOrigin = RoomOrigin (0,0)  // TODO: Choose random start room.
            LevelModel =
                {
                    LevelIndex       = LevelIndex levelIndex
                    LevelTileMatrix  = levelMatrix
                    TileMatrixTraits = LevelTileMatrixDetails ()
                }
        }

    let manCentre = ViewPoint { ptx=220.0F<epx> ; pty=100.0F<epx> } // TODO: decide from positioner?

    let exclusionRectangles = [ManExclusionRectangleAround manCentre] // TODO: Need greater surround gap.

    // let placesForAdversariesInThisRoom = 
    //     AvailableObjectPositionsWithinRoom roomReference exclusionRectangles LargestAdversaryDimension |> Seq.toArray

    let screenModel =
        {
            InnerScreenModel =
                {
                    RoomReference      = roomReference
                    ScreenScore        = betweenScreenStatus.ScoreAndHiScore
                    ManInventory       = []
                    ManLives           = ManLives InitialLives
                    Interactible       = // TODO: Decide positions
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
                    ImageLookupsTables = StyleResources levelIndex
                    WhereToOnGameOver = whereToOnGameOver
                }

            ScreenMan =
                {
                    ManState          = ManStandingFacing EightWayDirection.Down8
                    ManCentrePosition = manCentre
                }

            ScreenDroids = 
                [] // Let's not:  NewDroidsForRoom roomReference.LevelModel.LevelIndex placesForAdversariesInThisRoom gameTime

            ScreenGhost          = NoGhostUntil (gameTime + GhostGraceDuration)
            ManBullets           = []
            DroidBullets         = []
            DecorativeFlickbooks = []
        }

    screenModel


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Apply level flip
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let WithLevelChangeApplied gameTime model =

    // TODO: de-structure

    let (LevelIndex levelIndex) = 
        model.InnerScreenModel.RoomReference.LevelModel.LevelIndex

    let levelIndex = 
        levelIndex + 1

    let whereToOnGameOver = 
        model.InnerScreenModel.WhereToOnGameOver

    let scoreAndHiScore =
        model.InnerScreenModel.ScreenScore

    let (ManLives lives) =
        model.InnerScreenModel.ManLives

    let betweenScreenStatus =
        {
            ScoreAndHiScore = scoreAndHiScore
            Lives           = lives
        }

    ModelForStartingLevel levelIndex whereToOnGameOver betweenScreenStatus gameTime


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Apply room flip
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let WithRoomFlipAppliedFrom roomFlipData gameTime model =

    let {
            NewRoomManCentre    = manCentreInNewRoom
            NewRoomOrigin       = newRoomOrigin
        } = roomFlipData

    let exclusionRectangles =
        [ManExclusionRectangleAround manCentreInNewRoom]

    let roomReference = 
        {
            RoomOrigin = newRoomOrigin
            LevelModel = model.InnerScreenModel.RoomReference.LevelModel
        }

    let placesForAdversariesInThisRoom = 
        AvailableObjectPositionsWithinRoom roomReference exclusionRectangles LargestAdversaryDimension |> Seq.toArray

    let bonus =
        match model.ScreenDroids with   
            | [] -> ScoreBonusForShootingAllDroids
            | _  -> 0u

    let model =
        {
            InnerScreenModel = 
                { 
                    model.InnerScreenModel with 
                        RoomReference = roomReference 
                        ScreenScore   = model.InnerScreenModel.ScreenScore |> ScoreIncrementedBy bonus
                }
            ScreenMan =
                {
                    ManState          = model.ScreenMan.ManState
                    ManCentrePosition = manCentreInNewRoom
                }
            ScreenDroids         = NewDroidsForRoom roomReference.LevelModel.LevelIndex placesForAdversariesInThisRoom gameTime
            ScreenGhost          = NoGhostUntil (gameTime + GhostGraceDuration)
            ManBullets           = []
            DroidBullets         = []
            DecorativeFlickbooks = []
        }

    model


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Apply in-game changes required
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    
let private WithTheFollowingStateApplied  // TODO: rename for clarification, because this is only applied when the man is alive
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
                    |> List.append additionalExplosions1 
                    |> List.append additionalExplosions2
        }

    model




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Screen state advance on frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Return a key-state-getter function that inhibits the player's keys
/// but will pass through all others (eg: PAUSE).
let InhibitingPlayerKeys (keyStateGetterToOverride : WebBrowserKeyCode -> InputEventKeyState) =

    let filteredKeyStateGetter wkey = 
        let (WebBrowserKeyCode key) = wkey
        if key=90 || key=37 || key=39 || key=38 || key=40 then  // TODO: code constants
            { JustDown=false ; Held = false }  // Lie about the real key state
        else
            keyStateGetterToOverride wkey

    filteredKeyStateGetter



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
            RoomOrigin        = roomOrigin
            LevelModel        = levelModel
        } = roomReference

    let {
            LevelIndex      = levelNumber
            LevelTileMatrix  = _
            TileMatrixTraits = _
        } = levelModel

    let roomNumber = RoomNumberFromRoomOrigin roomOrigin


    let normalGamePlay () =

        let keyStateGetter =
            match man.ManState with
                | ManStandingFacing _ 
                | ManWalking        _ -> keyStateGetter
                | ManElectrocuted
                | ManDead             -> keyStateGetter |> InhibitingPlayerKeys

        let manBullets   = manBullets   |> AdvancedWithBulletsRemovedThatHitWallsOrOutsidePlayArea roomReference
        let droidBullets = droidBullets |> AdvancedWithBulletsRemovedThatHitWallsOrOutsidePlayArea roomReference
        let decoratives  = decoratives  |> WithCompletedFlickbooksRemoved gameTime
        let droids       = droids       |> MovedToNewPositionsWhileConsidering (VPManCentreOf man) roomReference gameTime

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



    let manCentre = ManCentreOf man

    match manCentre |> CheckForNextLevel levelNumber inventory interactibles roomNumber with

        | true ->
            let switchToNextLevel gameTime =
                model
                    |> WithLevelChangeApplied gameTime
                    |> ReplacesModelIn gameState

            model 
                |> ReplacesModelIn gameState
                |> FrozenInTimeAt gameTime
                |> UntilFutureTimeAndThen (gameTime + LevelExitPauseDuration) switchToNextLevel


        | false ->
            match CheckForRoomFlip roomOrigin man with
                | Some roomFlipData ->
                    model 
                        |> WithRoomFlipAppliedFrom roomFlipData gameTime
                        |> ReplacesModelIn gameState

                | None ->
                    normalGamePlay () 
                        |> ReplacesModelIn gameState









// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  New screen constructor
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMissionIIScreen whereToOnGameOver (betweenScreenStatus:BetweenScreenStatus) gameTime =

    let screenModel = ModelForStartingLevel 0 whereToOnGameOver betweenScreenStatus gameTime

    NewGameState NextMissionIIScreenState RenderMissionIIScreen screenModel



