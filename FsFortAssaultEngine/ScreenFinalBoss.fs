module ScreenFinalBoss

// TODO: The gun elevation at 0 degrees fires straight across.  This looks weird.

open Angle
open Time
open DrawingShapes
open DrawingFunctions
open SharedDrawing
open ScoreHiScore
open Geometry
open Mechanics
open ResourceIDs
open Gun
open ScorePanel
open FlickBook
open Collisions
open FinalBossAndTankBattleShared
open ImagesAndFonts
open StaticResourceAccess
open InputEventData
open ScreenHandler

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private TargetTriggerDistance     =   3.0F<epx>
let private InitialPlayerGunPositionX = 160.0F<epx>
let private BossGunCentrePosition     = { ptx=160.0F<epx> ; pty=20.0F<epx> }
let private PauseTimeWhenEnded        =   4.0F<seconds>
let private BossAnimationDuration     =  20.0F<seconds>
let private ExplosionDuration         =   0.75F<seconds>
let private FlagFlutterAnimDuration   =   0.5F<seconds>
let private ScoreForHittingTarget     =  2000u
let private GunStepRate               =   30.0F<degrees/seconds>
let private InitialGunElevation       =   30.0F<degrees>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private Imgs = Array.map ImageFromID

let private BossFlickBookType () =  // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = BossAnimationDuration
        FlickBookImages         = Imgs [|ImageFinalBoss0 ; ImageFinalBoss1 ; ImageFinalBoss2 ; ImageFinalBoss3 ; ImageFinalBoss4 ; ImageFinalBoss5 |]
        VisibilityBeforeStart   = Visible
        VisibilityAfterEnd      = Visible
    }

let private ExplosionFlickBookType () =  // TODO: Made into a function because of Fable static-initializer-order problem
    {
        FlickBookDuration       = ExplosionDuration
        FlickBookImages         = Imgs [| ImageShipExplode0 ; ImageShipExplode1 ; ImageShipExplode2 ; ImageShipExplode3 |]
        VisibilityBeforeStart   = Visible
        VisibilityAfterEnd      = Visible
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private AlliedState =
    | AlliedTankInPlay
    | WonScreen        of startTime : float32<seconds>
    | TankIsShot       of startTime : float32<seconds>
    | FinalBossScreenOver

type private FinalBossScreenModel =
    {
        FinalBossTargets           : FinalBossTargets
        ScoreAndHiScore            : ScoreAndHiScore
        TanksRemaining             : uint32
        ScreenStartTime            : float32<seconds>
        GunAim                     : GunAim
        AlliedState                : AlliedState
        Explosions                 : FlickBookInstance list
        BossGunFlickBook           : FlickBookInstance
        WhereToGoOnGameOver        : ScoreAndHiScore -> ErasedGameState
        WhereToOnVictory           : ScoreAndHiScore -> float32<seconds> -> ErasedGameState
        WhereToOnTankDestroyed     : int -> uint32 -> ScoreAndHiScore -> FinalBossTargets -> float32<seconds> -> ErasedGameState
        MapNumber                  : int
    }

let private SurrenderImages () =   // TODO: Made into a function because of Fable static-initializer-order problem
    Imgs [| ImageFinalBossSurrender0 ; ImageFinalBossSurrender1 |]

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private BossHasFired model gameTime =

    let elapsed = gameTime - model.ScreenStartTime
    elapsed > BossAnimationDuration

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NewExplosion centreLocation gameTime =
    {
        FlickBookType            = ExplosionFlickBookType ()
        FlickBookMechanicsObject = MechanicsControlledStationaryObject centreLocation gameTime ExplosionDuration
        FlickBookStartTime       = gameTime
    }

let private ResultOfWhateverShellsHitTheFort shells (targets:Target list) explosions score gameTime =

    let shellCollidesWithFort gameTime (shell:Shell) target =
        if target.TargetLocation = targets.Head.TargetLocation then  // Can only hit the head item!
            if shell.ShellMechanicsObject |> IsMOMStillInPlayAt gameTime then
                false // because we only want to consider hit-testing when the projectile has played through to its end.
            else
                shell.ShellMechanicsObject.FinalPosition |> IsWithinRegionOf (target.TargetLocation) TargetTriggerDistance
        else
            false

    let createExplosionAndScoreFor (shell:Shell) =
        let shellPos = shell.ShellMechanicsObject |> MOMPositionAt gameTime
        (NewExplosion shellPos gameTime, ScoreForHittingTarget)

    ResultOfProjectileCollisions
        shells
        targets
        (shellCollidesWithFort gameTime)
        (fun x -> x.ShellStartTime)
        (fun x -> x.TargetLocation.ptx)
        explosions
        score
        createExplosionAndScoreFor

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderFinalBossScreen render (model:FinalBossScreenModel) gameTime =

    let imgBack = ImageFinalBossBackground |> ImageFromID

    let DrawBackground () =
        Image1to1 render 0<epx> 0<epx> imgBack

    let DrawTargets targetList =
        let rec drawTargets isActiveTarget targetList =
            match targetList with
                | [] -> ()
                | {TargetLocation=pos}::tail ->
                    let targetImage = if isActiveTarget then ImageFinalBossActiveTarget else ImageFinalBossTarget
                    CentreImage render pos.ptx pos.pty (targetImage |> ImageFromID)
                    drawTargets false tail
        drawTargets true targetList

    let DrawBossGun () =
        DrawFlickbookInstance render model.BossGunFlickBook gameTime

    let DrawBlackout () =
        Rectangle render 0<epx> 0<epx> ScreenWidthInt ScreenHeightInt (SolidColour(0000000u))
        Text render RedFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (ScreenHeightInt / 2) "TANK DESTROYED"

    let DrawSurrender gameTime =
        let pos     = BossGunCentrePosition
        let elapsed = gameTime - model.ScreenStartTime
        CycleImages render pos.ptx pos.pty (SurrenderImages ()) FlagFlutterAnimDuration elapsed

    let h = imgBack.ImageMetadata.ImageHeight

    let DrawGun gameTime =
        Gun.DrawGun render (h |> IntToFloatEpx) model.GunAim gameTime

    match model.AlliedState with

        | AlliedTankInPlay ->
            DrawBackground ()
            DrawTargets model.FinalBossTargets.TargetsOnFinalBoss
            DrawBossGun ()
            DrawFlickbookInstanceList render model.Explosions gameTime
            DrawGun gameTime

        | WonScreen(_) ->
            DrawBackground ()
            DrawSurrender gameTime
            DrawFlickbookInstanceList render model.Explosions gameTime
            DrawGun gameTime

        | TankIsShot(_) ->
            DrawBlackout ()

        | FinalBossScreenOver ->
            ()  // Won't happen because the Storyboard switches.
            

    let scorePanel =
        {
            ScoreAndHiScore  = model.ScoreAndHiScore
            ShipsPending     = 0u
            ShipsThrough     = 0u
            Tanks            = model.TanksRemaining
            Damage           = 0u
            MaxDamage        = 0u
            PlaneIntel       = None
            Elevation        = model.GunAim.GunElevation      
        }

    ScoreboardArea render h
    DrawScorePanel render h scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private OldNewFinalBossScreen 
        scoreAndHiScore 
        tanksRemaining 
        finalBossTargets 
        mapNumber
        whereToGoOnGameOver 
        whereToOnVictory 
        whereToOnTankDestroyed 
        gameTime =
    {
        FinalBossTargets  = finalBossTargets

        ScoreAndHiScore   = scoreAndHiScore
        TanksRemaining    = tanksRemaining
        ScreenStartTime   = gameTime
        GunAim            = NewGunWithDefaultTraits SingleBarrelGun InitialPlayerGunPositionX InitialGunElevation GunStepRate gameTime
        AlliedState       = AlliedTankInPlay
        Explosions        = []
        BossGunFlickBook  =
            {
                FlickBookType            = BossFlickBookType ()
                FlickBookStartTime       = gameTime
                FlickBookMechanicsObject = MechanicsControlledStationaryObject BossGunCentrePosition gameTime BossAnimationDuration
            }
        WhereToGoOnGameOver    = whereToGoOnGameOver   
        WhereToOnVictory       = whereToOnVictory      
        WhereToOnTankDestroyed = whereToOnTankDestroyed
        MapNumber = mapNumber
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private OldNextFinalBossScreenState oldState keyStateGetter gameTime frameElapsedTime =

    let input = keyStateGetter |> DecodedInput

    match oldState.AlliedState with

        | AlliedTankInPlay ->   // TODO: I only got this logic right on the Tank Battle screen -- we don't clear flickbooks when the Tank is Shot.

            let elapsedSinceStart = gameTime - oldState.ScreenStartTime

            if oldState.FinalBossTargets.TargetsOnFinalBoss.IsEmpty then
                { oldState with AlliedState = WonScreen(gameTime) }
            
            elif elapsedSinceStart > BossAnimationDuration then
                { oldState with AlliedState = TankIsShot(gameTime) }
            
            else
                let gun        = oldState.GunAim
                let explosions = oldState.Explosions
                let targets    = oldState.FinalBossTargets.TargetsOnFinalBoss
                let score      = oldState.ScoreAndHiScore

                let explosions =
                    explosions |> WithCompletedFlickbooksRemoved gameTime

                let gunBaseY =
                    (ImageFinalBossBackground |> ImageFromID).ImageMetadata.ImageHeight |> IntToFloatEpx
                
                let gun =
                    UpdatedGunAimAccordingToInput input gameTime frameElapsedTime gunBaseY gun
                
                let shells, targets, explosions, score =
                    ResultOfWhateverShellsHitTheFort gun.Shells targets explosions score gameTime  // TODO: Unlike the AirBattle screen this REMOVES finished shells.  Should the boss screen be using ListWithCVODisappearedObjectsRemoved?

                let gun =
                    { gun with Shells = shells }  // TODO:  A shame to assume to re-bind this, but it's because we don't easily know (here) if the shells got changed.

                let gun =
                    gun |> UpdatedGunAimWithCompletedShellsRemoved gameTime 

                {
                    oldState with
                        GunAim           = gun
                        Explosions       = explosions
                        ScoreAndHiScore  = score
                        FinalBossTargets = { TargetsOnFinalBoss = targets }
                }

        | WonScreen(timeEnded)
        | TankIsShot(timeEnded) ->

            let elapsedSinceEnded = gameTime - timeEnded
            if elapsedSinceEnded > PauseTimeWhenEnded then
                { oldState with AlliedState = FinalBossScreenOver ; TanksRemaining = oldState.TanksRemaining - 1u }
            else
                oldState

        | FinalBossScreenOver ->
            oldState   // Ideology:  Never risk the logic rest of the logic when the screen is over.




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Adapter until above refactored
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextFinalBossScreenState gameState keyStateGetter gameTime elapsed =

    let model = ModelFrom gameState
    let model = OldNextFinalBossScreenState model keyStateGetter gameTime elapsed

    match model.AlliedState with

        | AlliedTankInPlay
        | WonScreen _
        | TankIsShot _ -> 
            gameState |> WithUpdatedModel model

        | FinalBossScreenOver ->
            if model.FinalBossTargets.TargetsOnFinalBoss.IsEmpty then
                model.WhereToOnVictory model.ScoreAndHiScore gameTime 

            elif model.TanksRemaining > 0u then
                model.WhereToOnTankDestroyed 
                    (model.MapNumber + 1) 
                    model.TanksRemaining 
                    model.ScoreAndHiScore 
                    model.FinalBossTargets 
                    gameTime

            else
                model.WhereToGoOnGameOver model.ScoreAndHiScore

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewFinalBossScreen 
    mapNumber scoreAndHiScore tanksRemaining finalBossAndTankBattleData 
    whereToOnGameOver whereToOnVictory whereToOnTankDestroyed gameTime =

    let bossModel =
        OldNewFinalBossScreen 
            scoreAndHiScore 
            tanksRemaining 
            finalBossAndTankBattleData 
            mapNumber
            whereToOnGameOver 
            whereToOnVictory
            whereToOnTankDestroyed
            gameTime 

    NewGameState NextFinalBossScreenState RenderFinalBossScreen bossModel

