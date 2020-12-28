module ScreenAirBattle

open Angle
open Time
open DrawingShapes
open SharedDrawing
open DrawingFunctions
open ScoreHiScore
open Geometry
open Mechanics
open ResourceIDs
open Gun
open ScorePanel
open FlickBook
open AirAndSeaBattleShared
open Collisions
open Rules
open ImagesAndFonts
open StaticResourceAccess
open InputEventData
open GameStateManagement

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let InitialGunElevation       =   30.0F<degrees>

#if SHORT_PLAYTHROUGH
let NumberOfRaidersWhenStrong =  10u  // Because we don't want to wait for ages.  We can also commit suicide in this mode.
#else
let NumberOfRaidersWhenStrong =  NumShipsAtInitialEngagement * 4u
#endif

let InitialPlayerGunPositionX =  160.0F<epx>
let PlaneTriggerDistance      =    6.0F<epx>   // TODO: This might need to be rectangular at the very least, certainly not square.  Also is a candidate for the game difficulty!
let PauseTimeWhenEnded        =    4.0F<seconds>
let ExplosionDuration         =    0.75F<seconds>
let ScoreForHittingPlane      = 2000u
let MaxDamagePerShip          =    5u
let PlaneDuration             =    6.0F<seconds>
let PlaneFiringTimeOffset     =    3.0F<seconds>
let SortieApproxInterval      =    5.0F<seconds>
let BombsDuration             =    1.0F<seconds>
let PlaneBombTargetY          =  150.0F<epx>  // TODO: Calculate somehow?
let MaxPlanesActiveAtOnce     =    3
let GunStepRate               =   30.0F<degrees/seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ToNumberOfRaiders enemyStrength =
    match enemyStrength with
        | StrongEnemy -> NumberOfRaidersWhenStrong
        | WeakerEnemy -> NumberOfRaidersWhenStrong / 2u

let ToPlaneIntelligenceBarCount n =
    (n + 3u) / 4u

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let Imgs arr = arr |> Array.map ImageFromID

let PlaneStraightOnFlickBookType () = // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = PlaneDuration
        FlickBookImages         = Imgs [| ImagePlane0 ; ImagePlane1 ; ImagePlane2 ; ImagePlane3 ; ImagePlane4 ; ImagePlane5 ; ImagePlane6 |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Visible
    }

let PlaneBankingLeftFlickBookType () =  // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = PlaneDuration
        FlickBookImages         = Imgs [| ImagePlane0 ; ImagePlane1 ; ImagePlane2 ; ImagePlane3 ; ImagePlane4BankingLeft ; ImagePlane5BankingLeft ; ImagePlane6BankingLeft |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Visible
    }

let PlaneBankingRightFlickBookType () =  // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = PlaneDuration
        FlickBookImages         = Imgs [| ImagePlane0 ; ImagePlane1 ; ImagePlane2 ; ImagePlane3 ; ImagePlane4BankingRight ; ImagePlane5BankingRight ; ImagePlane6BankingRight |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Visible
    }

let PlaneExplosionFlickBookType () =  // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = ExplosionDuration
        FlickBookImages         = Imgs [| ImagePlaneExplode0 ; ImagePlaneExplode1 ; ImagePlaneExplode2 |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Visible
    }

let PlaneBombsFlickBookType () =  // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = BombsDuration
        FlickBookImages         = Imgs [| ImagePlaneBomb0 ; ImagePlaneBomb1 ; ImagePlaneBomb2 ; ImagePlaneBomb3 ; ImagePlaneBomb4 |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Visible
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type Plane =
    {
        PlaneFlickBookInstance : FlickBookInstance
        PlaneHasReleasedBombs  : bool
    }

type AirBattleScreenModel =
    {
        ScoreAndHiScore : ScoreAndHiScore
        ShipsRemaining  : uint32
        Planes          : Plane list
        PlaneBombs      : FlickBookInstance list
        PlanesRemaining : uint32
        GunAim          : GunAim
        AlliedState     : AlliedState
        SkyExplosion    : FlickBookInstance list
        Explosions      : FlickBookInstance list
        EnemyShips      : EnemyShip list
        Damage          : uint32
        LastSortieAt    : float32<seconds>
        WhereToGoOnGameOver       : ScoreAndHiScore -> ErasedGameState
        WhereToOnCourseCompletion : uint32 -> ScoreAndHiScore -> float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let FlickbookFromPlane plane = plane.PlaneFlickBookInstance

let PlaneHasntFiredYet plane = (plane.PlaneHasReleasedBombs = false)

let IsAtOrAfterBombReleaseStage gameTime plane =

    let timeSincePlaneStarted =
        gameTime - plane.PlaneFlickBookInstance.FlickBookStartTime

    timeSincePlaneStarted >= PlaneFiringTimeOffset

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewExplosion centreLocation gameTime =
    {
        FlickBookType            = PlaneExplosionFlickBookType ()
        FlickBookMechanicsObject = MechanicsControlledStationaryObject centreLocation gameTime ExplosionDuration
        FlickBookStartTime       = gameTime
    }

let ShellCollidesWithPlane gameTime (shell:Shell) plane =

    if shell.ShellMechanicsObject |> IsMOMStillInPlayAt gameTime then
        false
    else
        let planePos = plane |> FlickbookFromPlane |> FlickBookPositionAtTime gameTime
        shell.ShellMechanicsObject.FinalPosition |> IsWithinRegionOf planePos PlaneTriggerDistance

let ResultOfWhateverShellsHitThePlanes shells planes explosions score gameTime =

    let createExplosionAndScoreFor (shell:Shell) =
        let shellPos = shell.ShellMechanicsObject |> MOMPositionAt gameTime
        (NewExplosion shellPos gameTime, ScoreForHittingPlane)

    ResultOfProjectileCollisions
        shells
        planes
        (ShellCollidesWithPlane gameTime)
        (fun x -> x.ShellStartTime) // Shell identity basis: They will all have different start times.
        (fun x -> x.PlaneFlickBookInstance.FlickBookStartTime) // Plane identity basis: They will all have different start times.
        explosions
        score
        createExplosionAndScoreFor


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawDebugPlaneHitTestRectangle render planes gameTime =

    planes |> List.iter (fun plane ->
        let {ptx=x;pty=y} = plane.PlaneFlickBookInstance |> FlickBookPositionAtTime gameTime
        let x,y = x |> FloatEpxToIntEpx , y |> FloatEpxToIntEpx
        let dist = PlaneTriggerDistance |> FloatEpxToIntEpx
        SquareAroundPoint render x y (dist * 2) (SolidColour(0xFF0000u))
    )

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderAirBattleScreen render (model:AirBattleScreenModel) gameTime =

    let DrawBackground () =
        Image1to1 render 0<epx> 0<epx> (ImageSeaBattleBackground0 |> ImageFromID)
        DrawFlickbookInstanceList render model.SkyExplosion gameTime

    let backgroundHeight = 
        (ImageSeaBattleBackground0 |> ImageFromID).ImageMetadata.ImageHeight  // They are all the same

    let DrawGun gameTime =
        Gun.DrawGun render (backgroundHeight |> IntToFloatEpx) model.GunAim gameTime

    let DrawBasicStuff () =
        DrawBackground ()
        DrawEnemyShips render model.EnemyShips gameTime
        DrawBuriedFlickbookInstanceList render FlickbookFromPlane model.Planes gameTime
        DrawFlickbookInstanceList render model.PlaneBombs gameTime
        DrawFlickbookInstanceList render model.Explosions gameTime
        //DrawDebugPlaneHitTestRectangle render model.Planes gameTime

    match model.AlliedState with

        | AlliedShipInPlay ->
            DrawBasicStuff ()
            DrawGun gameTime

        | WonScreen(_) ->
            DrawBasicStuff ()
            DrawGun gameTime
            DrawStageCompleteMessage render

        | ShipSinking(_) ->
            DrawBasicStuff ()
            DrawShipDestroyedMessage render

        | AirOrSeaBattleScreenOver ->
            ()  // Won't happen because the Storyboard switches.
            

    let scorePanel =
        {
            ScoreAndHiScore  = model.ScoreAndHiScore
            ShipsPending     = model.ShipsRemaining
            ShipsThrough     = 0u
            Tanks            = model.ShipsRemaining |> ToTankCountFromShipCount
            Damage           = model.Damage
            MaxDamage        = MaxDamagePerShip
            PlaneIntel       = Some (model.PlanesRemaining |> ToPlaneIntelligenceBarCount)

            Elevation        = model.GunAim.GunElevation      
        }

    ScoreboardArea render backgroundHeight
    DrawScorePanel render backgroundHeight scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// TODO: Various constants here

let PlaneFlickbookFor gameTime =

    let n = (int gameTime) % 4
    if   n = 0 then (PlaneBankingLeftFlickBookType,  -50.0F<epx>)
    elif n = 1 then (PlaneBankingRightFlickBookType, +50.0F<epx>)
    else            (PlaneStraightOnFlickBookType,     0.0F<epx>)

let NewPlane gameTime =

    let (planeFlickBook, dx) = PlaneFlickbookFor gameTime
    let startX = LanguagePrimitives.Float32WithMeasure<epx>( float32 (((int gameTime) % 24) * 10 + 40) )
    let endX   = startX + dx

    {
        PlaneFlickBookInstance = 
            {
                FlickBookType            = planeFlickBook ()
                FlickBookStartTime       = gameTime
                FlickBookMechanicsObject = 
                    MechanicsControlledMovingObject 
                        SpeedingUpMotion 
                        {ptx=startX ; pty=90.0F<epx>} {ptx=endX ; pty = -14.0F<epx>} gameTime PlaneDuration
            }

        PlaneHasReleasedBombs  = false
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPlaneBombFor planeThatIsFiring gameTime =

    let bombStartPos =
        planeThatIsFiring.PlaneFlickBookInstance |> FlickBookPositionAtTime gameTime

    let {ptx=x ; pty=y} = bombStartPos
    let bombEndPos = {ptx=x ; pty=PlaneBombTargetY}

    {
        FlickBookType            = PlaneBombsFlickBookType ()
        FlickBookStartTime       = gameTime
        FlickBookMechanicsObject = MechanicsControlledMovingObject SpeedingUpMotion bombStartPos bombEndPos gameTime BombsDuration
    }

let ConsiderReleasingNewBombsFromPlanes gameTime planeBombs planes =

    let folder (planeBombState,newPlanes) plane =

        if plane.PlaneHasReleasedBombs then
            (planeBombState, plane::newPlanes)

        elif plane |> IsAtOrAfterBombReleaseStage gameTime then
            let updatedBombsList  = (NewPlaneBombFor plane gameTime)::planeBombState
            let updatedPlanesList = { plane with PlaneHasReleasedBombs = true }::newPlanes
            (updatedBombsList, updatedPlanesList)

        else 
            (planeBombState, plane::newPlanes)

    List.fold folder (planeBombs, []) planes

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ConsiderLaunchingMorePlanes gameTime raidersLeft lastSortieAt (planes:Plane list) =

    if raidersLeft > 0u 
        && gameTime > (lastSortieAt + SortieApproxInterval)
        && planes.Length < MaxPlanesActiveAtOnce
    then
        (raidersLeft - 1u), gameTime, (NewPlane gameTime)::planes
    else
        raidersLeft, lastSortieAt, planes

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#if SHORT_PLAYTHROUGH
let CommitSuicideCheck damage (input:InputEventData.InputEventData) =
    if input.Left.Held && input.Right.Held && input.Down.Held && input.Fire.JustDown then
        (MaxDamagePerShip - 1u)  // So ONE plane at least must hit us
    else
        damage
#endif

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private OldNewAirBattleScreen enemyStrength scoreAndHiScore shipsRemaining whereToOnGameOver whereToOnCourseCompletion gameTime =
    {
        ScoreAndHiScore  = scoreAndHiScore
        ShipsRemaining   = shipsRemaining
        PlanesRemaining  = enemyStrength |> ToNumberOfRaiders
        GunAim           = NewGunWithDefaultTraits DoubleBarrelGun InitialPlayerGunPositionX InitialGunElevation GunStepRate gameTime
        AlliedState      = AlliedShipInPlay
        Explosions       = []
        SkyExplosion     = []
        EnemyShips       = DefaultEnemyShipsArrangement ()
        Planes           = []
        PlaneBombs       = []
        Damage           = 0u
        LastSortieAt     = 0.0F<seconds>
        WhereToGoOnGameOver       = whereToOnGameOver 
        WhereToOnCourseCompletion = whereToOnCourseCompletion
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private OldNextAirBattleScreenState oldState keyStateGetter gameTime frameElapsedTime =

    let input = keyStateGetter |> DecodedInput

    match oldState.AlliedState with

        | AlliedShipInPlay ->

            if oldState.PlanesRemaining = 0u && oldState.Planes.IsEmpty then
                { oldState with AlliedState = WonScreen(gameTime) }
            
            else
                let gunBaseY =
                    (ImageSeaBattleBackground0 |> ImageFromID).ImageMetadata.ImageHeight |> IntToFloatEpx

                let alliedState  = oldState.AlliedState
                let gun          = oldState.GunAim
                let explosions   = oldState.Explosions
                let skyExplosion = oldState.SkyExplosion
                let score        = oldState.ScoreAndHiScore
                let planes       = oldState.Planes
                let planeBombs   = oldState.PlaneBombs
                let damage       = oldState.Damage
                let lastSortieAt = oldState.LastSortieAt
                let raidersLeft  = oldState.PlanesRemaining

                let explosions =
                    explosions |> WithCompletedFlickbooksRemoved gameTime

                let skyExplosion =
                    skyExplosion |> WithCompletedFlickbooksRemoved gameTime

                let planes =
                    planes |> WithCompletedBuriedFlickbooksRemoved gameTime FlickbookFromPlane

                let bombCountBefore = planeBombs.Length

                let planeBombs =
                    planeBombs |> WithCompletedFlickbooksRemoved gameTime

                let bombCountAfter = planeBombs.Length

                let damage =
                    damage + uint32 (bombCountBefore - bombCountAfter)

                #if SHORT_PLAYTHROUGH
                let damage =
                    CommitSuicideCheck damage input
                #endif

                let gun =
                    UpdatedGunAimAccordingToInput input gameTime frameElapsedTime gunBaseY gun
                
                let planeBombs, planes =
                    ConsiderReleasingNewBombsFromPlanes gameTime planeBombs planes

                let raidersLeft, lastSortieAt, planes =
                    ConsiderLaunchingMorePlanes gameTime raidersLeft lastSortieAt planes

                let alliedState, skyExplosion =
                    if damage >= MaxDamagePerShip then
                        ShipSinking(gameTime), [NewSkyExplosionFlickBook gameTime] // we don't want more than one SkyExplosion at once!
                    else
                        alliedState, skyExplosion

                let shells, planes, explosions, score =
                    ResultOfWhateverShellsHitThePlanes gun.Shells planes explosions score gameTime

                let gun =
                    { gun with Shells = shells }

                let gun =
                    gun |> UpdatedGunAimWithCompletedShellsRemoved gameTime 

                {
                    oldState with
                        AlliedState      = alliedState
                        GunAim           = gun
                        Explosions       = explosions
                        SkyExplosion     = skyExplosion
                        Planes           = planes
                        PlaneBombs       = planeBombs
                        ScoreAndHiScore  = score
                        Damage           = damage
                        LastSortieAt     = lastSortieAt
                        PlanesRemaining  = raidersLeft
                }

        | WonScreen(timeEnded) -> 

            let elapsedSinceEngagement = gameTime - timeEnded
            if elapsedSinceEngagement > PauseTimeWhenEnded then
                { oldState with AlliedState = AirOrSeaBattleScreenOver }
            else
                oldState

        | ShipSinking(timeEnded) ->

            let elapsedSinceEngagement = gameTime - timeEnded
            if elapsedSinceEngagement > PauseTimeWhenEnded then
                let numShips = oldState.ShipsRemaining
                if numShips > 1u then
                    { 
                        oldState with 
                            AlliedState = AlliedShipInPlay
                            ShipsRemaining = numShips - 1u
                            SkyExplosion = []
                            Damage = 0u 
                    }
                else
                    {
                        oldState with
                            AlliedState    = AirOrSeaBattleScreenOver
                            ShipsRemaining = 0u
                            SkyExplosion   = []
                    }
            else
                oldState

        | AirOrSeaBattleScreenOver ->
            oldState   // Ideology:  Never risk the logic rest of the logic when the screen is over.





// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Adapter until above refactored
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextAirBattleScreenState gameState keyStateGetter gameTime elapsed =

    let model = ModelFrom gameState
    let model = OldNextAirBattleScreenState model keyStateGetter gameTime elapsed

    match model.AlliedState with
        | AlliedShipInPlay
        | ShipSinking _
        | WonScreen _ -> 
            gameState |> WithUpdatedModel model

        | AirOrSeaBattleScreenOver ->
            if model.ShipsRemaining > 0u then 
                model.WhereToOnCourseCompletion 
                    model.ShipsRemaining
                    model.ScoreAndHiScore 
                    gameTime
            else 
                model.WhereToGoOnGameOver
                    model.ScoreAndHiScore 
                    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewAirBattleScreen 
        enemyStrength 
        scoreAndHiScore 
        shipsRemaining 
        whereToOnGameOver 
        whereToOnCourseCompletion 
        gameTime =

    let airBattleModel =
        OldNewAirBattleScreen 
            enemyStrength 
            scoreAndHiScore 
            shipsRemaining 
            whereToOnGameOver 
            whereToOnCourseCompletion 
            gameTime

    NewGameState NextAirBattleScreenState RenderAirBattleScreen airBattleModel

