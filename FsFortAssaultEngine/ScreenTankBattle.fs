module ScreenTankBattle

open TankMapFileLoader // TODO:  only needed for the constants
open Time
open Geometry
open Tiles
open ScoreHiScore
open ScorePanel
open ResourceIDs
open DrawingFunctions
open FlickBook
open BeachBackgroundRenderer
open InputEventData
open Mechanics
open Collisions
open Algorithm
open FinalBossAndTankBattleShared
open ImagesAndFonts
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NumTilesVertically              = NumberOfRowsForTankMatrix
let TileSquareSide                  =  16<epx>

let ScrollingSectionTopY            = 196<epx> - (NumTilesVertically * TileSquareSide)
let TankTopLimitY                   =  60.0F<epx>
let TankBottomLimitY                = 188.0F<epx>
let TankStartY                      = (TankTopLimitY + TankBottomLimitY) / 2.0F
let TankX                           = ScreenWidth - 16.0F<epx>  // TODO: dimension related to tank width.
let TankGunOffsetFromCentreX        = 12.0F<epx>  // TODO: Need a metadata file so artist could alter these.
let TankGunOffsetFromCentreY        =  3.0F<epx>  // TODO: Need a metadata file so artist could alter these.
let EnemyTankGunOffsetFromCentreX   = 12.0F<epx>  // TODO: Need a metadata file so artist could alter these.
let EnemyTankGunOffsetFromCentreY   =  1.0F<epx>  // TODO: Need a metadata file so artist could alter these.
let EnemyTankFiringInterval         =  3.0F<seconds>

let PauseTimeWhenEnded              =   4.0F<seconds>
let WholeScreenTime                 = 120.0F<seconds>
let TankMovementPerSecond           = 15.0F<epx/seconds>
let TankReFireInterval              =  2.5F<seconds>
let TankTracksAnimDuration          =  0.2F<seconds>
let ExplosionDuration               =  0.75F<seconds>

let ScoreForHittingEnemyTank        = 1000u
let MissileCollisionTriggerDistance =  7.0F<epx>
let ReachedFortTileCount            = 4   // Count includes the extra lead-in/lead-out spaces that are automatically added.  (try 45 tto truncate the screen)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let Imgs = Array.map ImageFromID

let ExplosionFlickBookType () =  // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = ExplosionDuration
        FlickBookImages         = Imgs [| ImageShipExplode0 ; ImageShipExplode1 ; ImageShipExplode2 ; ImageShipExplode3 |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Hidden
    }

let NewExplosion centreLocation gameTime =
    {
        FlickBookType            = ExplosionFlickBookType ()
        FlickBookStartTime       = gameTime
        FlickBookMechanicsObject = MechanicsControlledStationaryObject centreLocation gameTime ExplosionDuration
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type TankDirection = TankFacingUpLeft | TankFacingLeft | TankFacingDownLeft

type AlliedState =
    | AlliedTankInPlay     of tankY:float32<epx> * TankDirection
    | AlliedTankExploding  of startTime:float32<seconds>
    | AlliedTankReachedFort
    | AlliedTankDestroyed

type EnemyTankMatrixLocation =
    {
        etmx : float32<epx>
        etmy : float32<epx>
        // TODO: have a tank kind?
    }

type TankBattleScreenConstantsModel =  // TODO: Use this convention in other screens?
    {
        ScreenStartTime            : float32<seconds>
        TileMatrixTraits           : TileMatrixTraits
        LevelMap                   : TankBattleMapMatrix
        FinalBossAndTankBattleData : FinalBossAndTankBattleData
    }

type TankBattleScreenModel =
    {
        // TODO: Adjust difficulty for enemy strength factor ?

        /// Things that do NOT change during this level.
        Constants          : TankBattleScreenConstantsModel

        ScoreAndHiScore    : ScoreAndHiScore
        TanksRemaining     : uint32
        AlliedState        : AlliedState
        Decoratives        : FlickBookInstance list   // NB: All offset by the scrolling of course!
        AlliedMissiles     : FlickBookInstance list   // No scroll offsetting required
        EnemyMissiles      : FlickBookInstance list   // No scroll offsetting required
        EnemyTankLocations : EnemyTankMatrixLocation list
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Increment the map number for the next time we come into the tank battle screen.
let WithIncrementedMapNumber tankBattleScreenModel =

    // I don't desire to re-bind the constants every frame for state that
    // only changes at the very end of the level.

    {
        tankBattleScreenModel with
            Constants =
                {
                    tankBattleScreenModel.Constants with
                        FinalBossAndTankBattleData =
                            {
                                tankBattleScreenModel.Constants.FinalBossAndTankBattleData with
                                    TankBattleMapNumber =
                                        tankBattleScreenModel.Constants.FinalBossAndTankBattleData.TankBattleMapNumber + 1
                            }
                }
    }


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let MissileFlickbookType () = // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration     = 3.0F<seconds>
        FlickBookImages       = [| ImageTorpedo0 |> ImageFromID |]
        VisibilityBeforeStart = Hidden
        VisibilityAfterEnd    = Hidden
    }

let NewMissile originX originY gameTime =
    {
        FlickBookType            = MissileFlickbookType ()
        FlickBookStartTime       = gameTime
        FlickBookMechanicsObject = 
            MechanicsControlledMovingObject
                LinearMotion
                { ptx = originX ; pty = originY }
                { ptx = originX - ScreenWidth ; pty = originY }
                gameTime
                TankReFireInterval
    }

let NewEnemyMissile centreLocation gameTime =
    let { ptx=originX ; pty=originY } = centreLocation
    let originX = originX + EnemyTankGunOffsetFromCentreX
    let originY = originY - EnemyTankGunOffsetFromCentreY
    {
        FlickBookType            = MissileFlickbookType ()
        FlickBookStartTime       = gameTime
        FlickBookMechanicsObject = 
            MechanicsControlledMovingObject
                LinearMotion
                { ptx = originX ; pty = originY }
                { ptx = originX + ScreenWidth ; pty = originY }  // Doesn't really matter if it flies a screen width
                gameTime
                TankReFireInterval
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   
let TankCollisionRectangle (tankX:float32<epx>) (tankY:float32<epx>) tankDirection =
    match tankDirection with
        | TankFacingLeft     -> { Left = tankX - 8.0F<epx> ; Top = tankY - 4.0F<epx> ; Right = tankX + 8.0F<epx> ; Bottom = tankY + 4.0F<epx> }
        | TankFacingDownLeft
        | TankFacingUpLeft   -> { Left = tankX - 8.0F<epx> ; Top = tankY - 6.0F<epx> ; Right = tankX + 8.0F<epx> ; Bottom = tankY + 6.0F<epx> }

/// Offset of the LEFT edge of the whole matrix, measured from the view rectangle.
let TileMatrixOffsetXAtTimeOffset numTilesHorizontally (timeOffsetIntoLevel:float32<seconds>) =

    let s = int (timeOffsetIntoLevel * 5.0F) |> IntToIntEpx
    (-numTilesHorizontally * TileSquareSide) + ScreenWidthInt + s

let ScreenXtoMatrixPixelX numTilesHorizontally (timeOffsetIntoLevel:float32<seconds>) (x:float32<epx>) =

    let amountOfMatrixOffLeftOfScreen = -(TileMatrixOffsetXAtTimeOffset numTilesHorizontally timeOffsetIntoLevel)
    amountOfMatrixOffLeftOfScreen + (x |> FloatEpxToIntEpx)

let ScreenYtoMatrixPixelY (tankY:float32<epx>) =

    tankY - (ScrollingSectionTopY |> IntToFloatEpx)

let EnemyTankMatrixLocationToScreen numTilesHorizontally enemyLocation gameTime =

    let { etmx=ex ; etmy=ey } = enemyLocation
    let ofsX = TileMatrixOffsetXAtTimeOffset numTilesHorizontally gameTime |> IntToFloatEpx  // TODO: We repeat these calculations from elsewhere.
    let ofsY = ScrollingSectionTopY |> IntToFloatEpx
    { ptx=ofsX + ex ; pty=ofsY + ey }

let CanTankPassOverTile tileImageId =

    tileImageId = ImageTileSand || tileImageId = ImageTileBridge

let HasTankCrashed (tankY:float32<epx>) tankDirection (timeOffsetIntoLevel:float32<seconds>) (constants:TankBattleScreenConstantsModel) =

    let tiles = constants.LevelMap.TilesArray
    let numTilesHorizontally = constants.LevelMap.TilesHorizontally

    let tankScreenRectangle =
        TankCollisionRectangle TankX tankY tankDirection

    let tileMatrixViewportWindow =  // TODO: The following is not great
        {
            WindowLeft   = tankScreenRectangle.Left |> FloatEpxToIntEpx
            WindowTop    = tankScreenRectangle.Top  |> FloatEpxToIntEpx
            WindowWidth  = tankScreenRectangle |> RectangleWidth |> FloatEpxToIntEpx
            WindowHeight = tankScreenRectangle |> RectangleHeight |> FloatEpxToIntEpx
        }

    let tileMatrixOffset =  // Reminder - from the top left corner of the tileMatrixViewportWindow
        {
            OffsetX = (TileMatrixOffsetXAtTimeOffset numTilesHorizontally timeOffsetIntoLevel) 
                        - tileMatrixViewportWindow.WindowLeft

            OffsetY = -(tankScreenRectangle.Top - (ScrollingSectionTopY |> IntToFloatEpx)) |> FloatEpxToIntEpx
        }

    let mutable tankCanPassOver = true

    ForEachTileWithVisiblePortion 
        constants.TileMatrixTraits
        tileMatrixViewportWindow 
        tileMatrixOffset 
        (fun x y ix iy -> 
            let thisTile = tiles.[iy * numTilesHorizontally + ix]
            // DEBUG:   tiles.[iy * numTilesHorizontally + ix] <- TileBarricadeVisual
            tankCanPassOver <- tankCanPassOver && CanTankPassOverTile thisTile
        )

    not tankCanPassOver

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawMatrix render (constants:TankBattleScreenConstantsModel) (timeOffset:float32<seconds>) =

    let tileMatrixViewportWindow =
        {
            WindowLeft   = 0<epx>
            WindowTop    = ScrollingSectionTopY
            WindowWidth  = ScreenWidthInt
            WindowHeight = NumTilesVertically * TileSquareSide
        }

    let tiles = constants.LevelMap.TilesArray
    let numTilesHorizontally = constants.LevelMap.TilesHorizontally

    let tileMatrixOffset =  // Reminder - from the top left corner of the tileMatrixViewportWindow
        {
            OffsetX = TileMatrixOffsetXAtTimeOffset numTilesHorizontally timeOffset
            OffsetY = 0<epx>
        }

    ForEachTileWithVisiblePortion 
        constants.TileMatrixTraits
        tileMatrixViewportWindow 
        tileMatrixOffset 
        (fun x y ix iy -> 
            let tileImage = (tiles.[iy * numTilesHorizontally + ix]) |> ImageFromID
            Image1to1 render x y tileImage)


 // TODO: Made into functions because of Fable static-initializer-order problem:
let ImagesTankFacingLeft     () = Imgs [| ImageTankFacingLeft0     ; ImageTankFacingLeft1     |]
let ImagesTankFacingUpLeft   () = Imgs [| ImageTankFacingUpLeft0   ; ImageTankFacingUpLeft1   |]
let ImagesTankFacingDownLeft () = Imgs [| ImageTankFacingDownLeft0 ; ImageTankFacingDownLeft1 |]


let TankImagesFor tankDirection =
    match tankDirection with
        | TankFacingLeft     -> ImagesTankFacingLeft ()
        | TankFacingUpLeft   -> ImagesTankFacingUpLeft ()
        | TankFacingDownLeft -> ImagesTankFacingDownLeft ()



let ForEachEnemyTankScreenLocation numTilesHorizontally gameTime f enemyTanks =

    let ofsX = TileMatrixOffsetXAtTimeOffset numTilesHorizontally gameTime |> IntToFloatEpx
    let ofsY = ScrollingSectionTopY |> IntToFloatEpx

    enemyTanks |> List.iter (fun {etmx=x ; etmy=y} -> f (x + ofsX) (y + ofsY))


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderTankBattleScreen render (model:TankBattleScreenModel) gameTime =

    let gameTime = gameTime - model.Constants.ScreenStartTime  // TODO: Ideologically this should be called levelTime

    let numTilesHorizontally = model.Constants.LevelMap.TilesHorizontally

    let imgTankGun1 = (ImageEnemyTankGun1 |> ImageFromID)

    let DrawEnemyTanks render enemyTanks gameTime =
        enemyTanks 
            |> ForEachEnemyTankScreenLocation numTilesHorizontally gameTime (fun x y -> 
                CentreImage render x y imgTankGun1)

    let DrawBasicStuff () =
        RenderBeachBackground render gameTime
        DrawMatrix render model.Constants gameTime
        DrawFlickbookInstanceList render model.AlliedMissiles gameTime // no need to offset by scrolling
        DrawFlickbookInstanceList render model.EnemyMissiles gameTime  // no need to offset by scrolling
        DrawEnemyTanks render model.EnemyTankLocations gameTime

    let DrawDecoratives () =
        DrawFlickbookInstanceList render model.Decoratives gameTime // TODO: Consider - offset by the scrolling?

    let DrawAlliedTank tankY tankDirection =
        let elapsed = gameTime - model.Constants.ScreenStartTime
        CycleImages render TankX tankY (TankImagesFor tankDirection) TankTracksAnimDuration elapsed

    let h = 0.0F<epx>
        
    match model.AlliedState with

        | AlliedTankInPlay(tankY, tankDirection) ->
            DrawBasicStuff ()
            DrawAlliedTank tankY tankDirection
            DrawDecoratives ()
            // let r = TankCollisionRectangle TankX tankY tankDirection
            // Rectangle render r.Left r.Top (r |> RectangleWidth) (r |> RectangleHeight) 0xFFFFFFu

        | AlliedTankExploding(_) ->
            DrawBasicStuff ()
            DrawDecoratives ()

        | AlliedTankReachedFort
        | AlliedTankDestroyed ->
            ()  // Won't happen because the Storyboard switches.

    DrawTankBattleScorePanel render (h |> FloatEpxToIntEpx) model.ScoreAndHiScore.Score model.TanksRemaining

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ToEnemyTankLocationsOnMatrix enemyTankTileLocations =

    let half = (TileSquareSide / 2)

    enemyTankTileLocations |> List.map (
        fun {etx=tileX ; ety=tileY} ->
            {
                etmx = IntToFloatEpx (tileX * TileSquareSide + half)
                etmy = IntToFloatEpx (tileY * TileSquareSide + half)
            }
    )


let ChooseCourseMap (tankMapsList:TankBattleMapMatrix list) tankBattleMapNumber =

    let n = tankMapsList.Length
    if n < 1 then failwith "List of tank battle maps should never be empty"
    tankMapsList.[tankBattleMapNumber % n]


let NewTankBattleScreen scoreAndHiScore tanksRemaining finalBossAndTankBattleData tankMapsList gameTime =

    let levelMap =
        ChooseCourseMap tankMapsList finalBossAndTankBattleData.TankBattleMapNumber

    let tileMatrixTraits =
        {
            TilesHorizontally = levelMap.TilesHorizontally
            TilesVertically   = NumTilesVertically
            TileWidthPixels   = TileSquareSide
            TileHeightPixels  = TileSquareSide
        }

    let constants = 
        {
            ScreenStartTime            = gameTime
            TileMatrixTraits           = tileMatrixTraits
            LevelMap                   = levelMap
            FinalBossAndTankBattleData = finalBossAndTankBattleData
        }

    {
        Constants          = constants
        ScoreAndHiScore    = scoreAndHiScore
        TanksRemaining     = tanksRemaining
        AlliedState        = AlliedTankInPlay(TankStartY, TankFacingLeft)
        Decoratives        = []
        AlliedMissiles     = []
        EnemyMissiles      = []
        EnemyTankLocations = levelMap.EnemyTanks |> ToEnemyTankLocationsOnMatrix
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let TankDirectionFromInput (input:InputEventData) =

    let up   = input.Up.Held
    let down = input.Down.Held

    if up && down then TankFacingLeft
    else if up    then TankFacingUpLeft
    else if down  then TankFacingDownLeft
    else               TankFacingLeft

let TankYMovedByDirection oldY tankDirection frameElapsedTime =

    let distance = frameElapsedTime * TankMovementPerSecond

    match tankDirection with
        | TankFacingUpLeft   -> max (oldY - distance) TankTopLimitY
        | TankFacingDownLeft -> min (oldY + distance) TankBottomLimitY
        | TankFacingLeft     -> oldY

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let CanFireNow missilesList reFireInterval gameTime =

    match missilesList |> FlickBookHeadItemStartTime with
        | None -> true
        | Some(mostRecentFiringTime) -> (gameTime - mostRecentFiringTime) >= reFireInterval


let ConsiderTankFiring tankY input alliedMissiles gameTime =

    if input.Fire.JustDown && CanFireNow alliedMissiles TankReFireInterval gameTime then

        let missileX =  TankX - TankGunOffsetFromCentreX
        let missileY =  tankY - TankGunOffsetFromCentreY

        (NewMissile missileX missileY gameTime)::alliedMissiles

    else
        alliedMissiles

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let AlliedMissileCollidesWithEnemyTank numTilesHorizontally gameTime alliedMissile enemyLocation =

    let enemyScreenLocation = 
        EnemyTankMatrixLocationToScreen numTilesHorizontally enemyLocation gameTime

    alliedMissile 
        |> FlickBookPositionAtTime gameTime 
        |> IsWithinRegionOf enemyScreenLocation MissileCollisionTriggerDistance


let EnemyMissileCollidesWithPlayer gameTime enemyMissile tankY =
    
    let thePlayer = { ptx=TankX ; pty=tankY }
    
    enemyMissile 
        |> FlickBookPositionAtTime gameTime 
        |> IsWithinRegionOf thePlayer MissileCollisionTriggerDistance


let DoesPlayerCollideWithEnemyTank numTilesHorizontally gameTime enemyTanks playerTankY playerTankDirection =
    
    let r = TankCollisionRectangle TankX playerTankY playerTankDirection
    
    let imgGun1 = (ImageEnemyTankGun1 |> ImageFromID).ImageMetadata

    let EnemyTankRectangleFor x y =

        let x' = x - ((imgGun1.ImageWidth |> IntToFloatEpx) / 2.0F)
        let y' = y - ((imgGun1.ImageHeight |> IntToFloatEpx) / 2.0F)

        {
            Left   = x'
            Top    = y'
            Right  = x' + (imgGun1.ImageWidth  |> IntToFloatEpx)
            Bottom = y' + (imgGun1.ImageHeight |> IntToFloatEpx)
        }

    let mutable collides = false

    enemyTanks 
        |> ForEachEnemyTankScreenLocation numTilesHorizontally gameTime 
                (fun x y -> 
                    collides <- collides || r |> RectangleIntersects (EnemyTankRectangleFor x y))

    collides

let MissileExplosionFor missile gameTime = 
    let missilePos = missile.FlickBookMechanicsObject |> MOMPositionAt gameTime
    NewExplosion missilePos gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMissileFiredBy enemyTankMatrixLocation numTilesHorizontally gameTime =

    let enemyScreenLocation = 
        EnemyTankMatrixLocationToScreen numTilesHorizontally enemyTankMatrixLocation gameTime

    NewEnemyMissile enemyScreenLocation gameTime


let EnemyMissilesWithAdditionalFirings numTilesHorizontally (enemyTanks:EnemyTankMatrixLocation list) (enemyMissiles:FlickBookInstance list) gameTime =

    // Which enemies are visible?  Only those can be considered for firing.
    // When was the most recent firing time?  Only consider firing again after a time period.

    let tankChosenFrom =
        ChooseItemFromListByModulo gameTime

    let matrixLeftPixel  =
        0.0F<epx> |> ScreenXtoMatrixPixelX numTilesHorizontally gameTime |> IntToFloatEpx

    let matrixRightPixel =
        ScreenWidth |> ScreenXtoMatrixPixelX numTilesHorizontally gameTime |> IntToFloatEpx

    let whereEnemyTankIsVisible {etmx=x ; etmy=_} =
        x >= matrixLeftPixel && x <= matrixRightPixel

    let thatAreVisible tanks =
        tanks |> PlanetSavingListFilter whereEnemyTankIsVisible

    let mostRecentFiringTimeOrDefault enemyMissiles defaultTime =
        match enemyMissiles with
            | [] -> defaultTime
            | head::_ -> head.FlickBookStartTime

    let itsTimeToConsiderFiringAgain enemyMissiles gameTime interval =
        let lastFiredAt = mostRecentFiringTimeOrDefault enemyMissiles (gameTime - interval)
        (gameTime - lastFiredAt) >= interval

    if itsTimeToConsiderFiringAgain enemyMissiles gameTime EnemyTankFiringInterval then
        match tankChosenFrom (enemyTanks |> thatAreVisible) with  // TODO: Don't generate filtered list every frame!
            | None       -> enemyMissiles
            | Some(tank) -> (NewMissileFiredBy tank numTilesHorizontally gameTime)::enemyMissiles
    else
        enemyMissiles

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let HasInPlayTankReachedTheFort alliedState tankMatrixX =

    match alliedState with
        | AlliedTankDestroyed
        | AlliedTankReachedFort
        | AlliedTankExploding(_) ->
            alliedState

        | AlliedTankInPlay(_) ->
            if tankMatrixX < (ReachedFortTileCount * TileSquareSide) then
                AlliedTankReachedFort
            else
                alliedState


let IsItTimeForExplodingTankToBeDestroyed alliedState tanksRemaining gameTime =

    match alliedState with

        | AlliedTankDestroyed
        | AlliedTankReachedFort
        | AlliedTankInPlay(_) ->
            alliedState, tanksRemaining

        | AlliedTankExploding(timeEnded) ->
            let elapsedSinceCrashing = gameTime - timeEnded
            if elapsedSinceCrashing > PauseTimeWhenEnded then
                AlliedTankDestroyed, (tanksRemaining - 1u)
            else
                alliedState, tanksRemaining


let PlayerMovementAndCrashDetection alliedState decoratives frameElapsedTime input constants gameTime =

    match alliedState with

        | AlliedTankDestroyed
        | AlliedTankReachedFort
        | AlliedTankExploding(_) ->
            alliedState, decoratives

        | AlliedTankInPlay(tankY, tankDirection) ->

            let tankY = TankYMovedByDirection tankY tankDirection frameElapsedTime
            let tankDirection = TankDirectionFromInput input

            if HasTankCrashed tankY tankDirection gameTime constants then
                let tankLocation = { ptx=TankX ; pty=tankY }
                let decoratives  = (NewExplosion tankLocation gameTime)::decoratives
                AlliedTankExploding(gameTime), decoratives
            
            else
                AlliedTankInPlay(tankY, tankDirection), decoratives

let PlayerMissileFiring alliedState input alliedMissiles gameTime =

    match alliedState with
        | AlliedTankDestroyed          //
        | AlliedTankReachedFort        // Player cannot fire again in these states
        | AlliedTankExploding(_) ->    //
            alliedMissiles  

        | AlliedTankInPlay(tankY, _tankDirection) ->
            ConsiderTankFiring tankY input alliedMissiles gameTime

let HasPlayerBeenHitByEnemyMissiles alliedState enemyMissiles decoratives scoreAndHiScore gameTime =

    match alliedState with
        | AlliedTankReachedFort       //
        | AlliedTankDestroyed         // Player cannot be hit again in these states
        | AlliedTankExploding(_) ->   //
            enemyMissiles, alliedState, decoratives, scoreAndHiScore

        | AlliedTankInPlay(tankY, _) ->
            let enemyMissiles, survivalResult, decoratives, scoreAndHiScore =
                ResultOfProjectileCollisionsWithSingleTarget
                    enemyMissiles
                    tankY
                    (EnemyMissileCollidesWithPlayer gameTime)
                    FlickBookStartTimeOf  // will do as an identity-function, since there's time gaps between firings
                    decoratives
                    scoreAndHiScore
                    (fun projectile -> (MissileExplosionFor projectile gameTime), 0u)

            match survivalResult with
                | PlayerSurvives  -> enemyMissiles, alliedState, decoratives, scoreAndHiScore
                | PlayerDestroyed -> enemyMissiles, AlliedTankExploding(gameTime), decoratives, scoreAndHiScore

let HasInPlayTankCrashedIntoEnemyTanks alliedState decoratives enemyTanks numTilesHorizontally gameTime =

    match alliedState with
        | AlliedTankDestroyed          //
        | AlliedTankReachedFort        // Player cannot crash into enemy tanks in these states
        | AlliedTankExploding(_) ->    //
            alliedState, decoratives

        | AlliedTankInPlay(tankY, tankDirection) ->
            if DoesPlayerCollideWithEnemyTank numTilesHorizontally gameTime enemyTanks tankY tankDirection then
                let alliedState = AlliedTankExploding(gameTime)
                let tankCentre  = {ptx=TankX ; pty=tankY}
                let decoratives = (NewExplosion tankCentre gameTime)::decoratives
                alliedState, decoratives
            else
                alliedState, decoratives

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

[<Struct>]
type TankBattleChapterTransition =
    | StayOnTankBattleScreen          of newModel1 : TankBattleScreenModel
    | GameOverOnTankBattleScreen      of newModel2 : TankBattleScreenModel
    | TankCompletedCourseSuccessfully of newModel3 : TankBattleScreenModel
    | RestartTankBattle               of newModel4 : TankBattleScreenModel


let NextTankBattleScreenState oldState input gameTime frameElapsedTime =

    let newModel =

        match oldState.AlliedState with

            | AlliedTankReachedFort
            | AlliedTankDestroyed ->
                oldState   // Ideology:  Never risk the logic rest of the logic when the screen is over.

            | _ ->

                // Important to measure time from the screen start:
                let gameTime = gameTime - oldState.Constants.ScreenStartTime

                let numTilesHorizontally = oldState.Constants.LevelMap.TilesHorizontally

                let tankMatrixX = ScreenXtoMatrixPixelX numTilesHorizontally gameTime TankX

                let scoreAndHiScore = oldState.ScoreAndHiScore
                let decoratives     = oldState.Decoratives
                let alliedState     = oldState.AlliedState
                let alliedMissiles  = oldState.AlliedMissiles
                let enemyMissiles   = oldState.EnemyMissiles
                let tanksRemaining  = oldState.TanksRemaining
                let enemyTanks      = oldState.EnemyTankLocations

                // Removal of completed flickbooks

                let decoratives     = decoratives    |> WithCompletedFlickbooksRemoved gameTime
                let alliedMissiles  = alliedMissiles |> WithCompletedFlickbooksRemoved gameTime
                let enemyMissiles   = enemyMissiles  |> WithCompletedFlickbooksRemoved gameTime

                // Rules:

                let alliedState =
                    HasInPlayTankReachedTheFort alliedState tankMatrixX

                let alliedState, tanksRemaining =
                    IsItTimeForExplodingTankToBeDestroyed alliedState tanksRemaining gameTime

                let alliedState, decoratives =
                    PlayerMovementAndCrashDetection alliedState decoratives frameElapsedTime input oldState.Constants gameTime

                let alliedMissiles =
                    PlayerMissileFiring alliedState input alliedMissiles gameTime

                let alliedMissiles, enemyTanks, decoratives, scoreAndHiScore =
                    ResultOfProjectileCollisions
                        alliedMissiles // Do the player's missiles hit any enemies?
                        enemyTanks
                        (AlliedMissileCollidesWithEnemyTank numTilesHorizontally gameTime)
                        FlickBookStartTimeOf  // will do as identity since there's time gaps between firings
                        id // TODO: Using the whole record as identity.  Will do for now.
                        decoratives
                        scoreAndHiScore
                        (fun projectile -> (MissileExplosionFor projectile gameTime), ScoreForHittingEnemyTank)

                let enemyMissiles, alliedState, decoratives, scoreAndHiScore =
                    HasPlayerBeenHitByEnemyMissiles alliedState enemyMissiles decoratives scoreAndHiScore gameTime

                let alliedState, decoratives =
                    HasInPlayTankCrashedIntoEnemyTanks alliedState decoratives enemyTanks numTilesHorizontally gameTime

                let enemyMissiles =
                    EnemyMissilesWithAdditionalFirings numTilesHorizontally enemyTanks enemyMissiles gameTime

                // TODO:  Consider moving enemy tanks -- not essential for MVP

                {
                    Constants          = oldState.Constants // Never updated during this level.

                    ScoreAndHiScore    = scoreAndHiScore
                    AlliedState        = alliedState
                    Decoratives        = decoratives    
                    AlliedMissiles     = alliedMissiles 
                    EnemyMissiles      = enemyMissiles  
                    TanksRemaining     = tanksRemaining
                    EnemyTankLocations = enemyTanks
                }

    match newModel.AlliedState with
        
        | AlliedTankReachedFort ->
            newModel |> WithIncrementedMapNumber

        | AlliedTankDestroyed
        | AlliedTankInPlay _
        | AlliedTankExploding _ ->
            newModel


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Query functions for Storyboard
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type TankBattleAfterFrameCase = StayOnTankBattleScreen | TankCompletedCourseSuccessfully | TankBattleGameOver | RestartTankBattle

let TankBattleTransition state =

    match state.AlliedState with
    
        | AlliedTankReachedFort ->
            TankCompletedCourseSuccessfully

        | AlliedTankDestroyed ->
            if state.TanksRemaining > 0u then RestartTankBattle else TankBattleGameOver
    
        | AlliedTankInPlay _
        | AlliedTankExploding _ ->
            StayOnTankBattleScreen

