﻿module ScreenSeaBattle

open Angle
open SharedDrawing
open DrawingCommandsEx
open ScoreHiScore
open Geometry
open Time
open Mechanics
open ImagesAndFonts
open Gun
open ScorePanel
open FlickBook
open AirAndSeaBattleShared
open Rules
open FontAlignment
open PendingEvents
open InputEventData
open StoryboardChapterChange


let InitialGunElevation            =   30.0F<degrees>
let InitialPlayerGunPositionX      =  160.0F<wu>
let PauseTimeWhenEnded             =    4.0F<seconds>
let ExplosionDuration              =    0.75F<seconds>
let IncomingOrdinanceDuration      =    6.0F<seconds>
let ScoreForSinkingEnemyShip       = 2000u
let MessageY                       =   15<wu>
let SplashDuration                 =    0.2F<seconds>
let TimeBetweenEnemyFirings        =    3.5F<seconds>  
let TimeBetweenEnemyLaunchGoingOffTopOfScreenAndComingBackOn = 0.3F<seconds>
let EnemyFireHittingShipY          =  155.0F<wu>
let EnemyFireMissingShipY          =  140.0F<wu>
let EnemyFireTriesCount            =    5
let SinkingShipFiringPauseDuration =    4.0F<seconds>
let GunStepRate                    =   10.0F<degrees/seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type DamageChange = DamageChange of unit

type HitsChange = { HitShipX : float32<wu> }  // identifies the enemy ship to be sunk

type IncomingChange = IncomingChange of unit

type SeaBattleScreenModel =
    {
        ScoreAndHiScore     : ScoreAndHiScore
        ShipsRemaining      : uint32
        GunAim              : GunAim
        AlliedState         : AlliedState
        AlliedCountToSink   : int
        EnemyShips          : EnemyShip list
        Decoratives         : FlickBookInstance list
        SkyExplosion        : FlickBookInstance list   // Slightly special treatment as I didn't want more than one going on at once.
        PendingIncoming     : Pending<IncomingChange> list
        PendingDamage       : Pending<DamageChange> list
        PendingHits         : Pending<HitsChange> list
        PendingMessageTexts : Pending<string> list
        PendingScoreChanges : Pending<uint32> list
        MessageText         : string
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let SplashFlickBookType = 
    {
        FlickBookDuration       = SplashDuration
        FlickBookImages         = [| ImageSplash0 |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Hidden
    }

let NewSplashFlickBook gameTime x y =
    {
        FlickBookType            = SplashFlickBookType
        FlickBookMechanicsObject = MechanicsControlledStationaryObject { xwf = x ; ywf = y } gameTime SplashDuration
        FlickBookStartTime       = gameTime
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let MappedToUnsinkingEnemyShipIn enemyShips gunX =

    enemyShips
        |> List.tryFind (fun ship -> 
            let halfShipWidth = ship.ShipImage.ImageWidth / 2.0F
            (Option.isNone ship.SinkStartTime)
                && (gunX |> IsWithinRangeOf ship.CentreX halfShipWidth))

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let WithEnemyShipSinking gameTime enemyShipX enemyShips =

    enemyShips 
        |> List.map (fun enemyShip -> 
            if enemyShip.CentreX = enemyShipX then
                { enemyShip with SinkStartTime = Some(gameTime) }
            else
                enemyShip)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let EnemyLaunchPart1FlickbookType =
    {
        FlickBookDuration     = 0.7F<seconds>
        FlickBookImages       = [| ImagePlaneBomb0 ; ImagePlaneBomb1 |]
        VisibilityBeforeStart = Hidden
        VisibilityAfterEnd    = Hidden
    }

let EnemyLaunchPart2FlickbookType =
    {
        FlickBookDuration     = 0.7F<seconds>
        FlickBookImages       = [| ImagePlaneBomb3 ; ImagePlaneBomb4 |]
        VisibilityBeforeStart = Hidden
        VisibilityAfterEnd    = Hidden
    }



let FlickbooksForEnemyLaunchFrom (ship:EnemyShip) decoratives willHit gameTime =

    let x       = ship.CentreX
    let originY = ship.BaseY - ship.ShipImage.ImageHeight
    let targetY = if willHit then EnemyFireHittingShipY else EnemyFireMissingShipY

    let t1 = gameTime
    let t2 = t1 + EnemyLaunchPart1FlickbookType.FlickBookDuration
    let t3 = t2 + TimeBetweenEnemyLaunchGoingOffTopOfScreenAndComingBackOn
    let t4 = t3 + EnemyLaunchPart2FlickbookType.FlickBookDuration
    let t5 = t4 + TimeBetweenEnemyFirings

    let launchFlickBookInstance =
        {
            FlickBookType            = EnemyLaunchPart1FlickbookType
            FlickBookStartTime       = t1
            FlickBookMechanicsObject = 
                MechanicsControlledMovingObject
                    SpeedingUpMotion
                    { xwf = x ; ywf = originY }
                    { xwf = x ; ywf = -10.0F<wu> }
                    t1
                    EnemyLaunchPart1FlickbookType.FlickBookDuration
        }

    let descentFlickBookInstance =
        {
            FlickBookType            = EnemyLaunchPart2FlickbookType
            FlickBookStartTime       = t3
            FlickBookMechanicsObject = 
                MechanicsControlledMovingObject
                    SpeedingUpMotion
                    { xwf = x ; ywf = -100.0F<wu> }
                    { xwf = x ; ywf = targetY }
                    t3
                    EnemyLaunchPart2FlickbookType.FlickBookDuration
        }

    let decoratives =
        if willHit then decoratives else (NewSplashFlickBook t4 x targetY)::decoratives

    let decoratives =
        launchFlickBookInstance::descentFlickBookInstance::decoratives

    decoratives, t4, t5



let rec PossiblyLaunchEnemyFire currentState _changeRequired gameTime =

    let (enemyShips , decoratives , _ , alliedCountToSink) = currentState

    match enemyShips with  // Only the head of the enemyShips list shall fire

        | [] -> PendingDone currentState // no change

        | ship::_ ->

            match ship.SinkStartTime with
                | None ->  // The head enemy ship is not sinking:

                    let willHit = (alliedCountToSink = 1)

                    let decoratives, t4, t5 = 
                        FlickbooksForEnemyLaunchFrom ship decoratives willHit gameTime

                    let futureDamage =
                        if willHit then [DamageChange () |> ToBeDoneAtTime t4] else []  // We don't care about completely resetting the futureDamage list.

                    let alliedCountToSink = alliedCountToSink - 1
                            
                    PendingDoneWithAdditionalToDos (
                        (enemyShips , decoratives , futureDamage , alliedCountToSink),
                        [IncomingChange () |> ToBeDoneAtTime t5]
                    )

                | Some(_) ->  // The head enemy ship is sinking.
                    PendingDoneWithAdditionalToDos (
                        currentState,
                        [IncomingChange () |> ToBeDoneAtTime (gameTime + SinkingShipFiringPauseDuration)]
                    )

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewSplashFlickbookForFiredGun gun hitTime = 
    
    let splashCentreY = 
        (gun.GunElevation |> ElevationToScreenY) - (ImageSplash0.ImageHeight / 2.0F) // TODO:  Here a fudge is needed, because the splash Flickbooks are drawn with centered sprites.
    
    let newSplashFlickbook = 
        NewSplashFlickBook hitTime gun.GunCentreX splashCentreY

    newSplashFlickbook


[<Struct>]
type GunAngleVersusShipType =
    | DirectHit
    | TooFarBy  of farBy:int
    | TooNearBy of nearBy:int



let GunAngleVersusShip gun ship =

    let diff = (int) (gun.GunElevation |> ReflectedElevation) - (int) ship.ElevationToHit
    if diff = 0 then DirectHit else if diff < 0 then TooNearBy (-diff) else TooFarBy (diff)



let ConsiderStateChangesForWhenPlayerFiresGun input gun enemyShips gameTime =

    if input.Fire.JustDown then
        
        let inline diffToMetres diff = 
            (abs diff) * 100

        let hitTime =
            gameTime + (gun.GunTraits.GunShellDuration * 2.0F)

        match gun.GunCentreX |> MappedToUnsinkingEnemyShipIn enemyShips with

            | None -> 
                None, None, None, Some (NewSplashFlickbookForFiredGun gun hitTime)

            | Some ship ->
                
                let playerShellResult =
                    GunAngleVersusShip gun ship

                let futureHit, futureMessageText, futureScoreChange = 
                    match playerShellResult with
    
                        | DirectHit ->
                            Some ({HitShipX = ship.CentreX} |> ToBeDoneAtTime hitTime),
                            Some ("DIRECT HIT" |> ToBeDoneAtTime hitTime), 
                            Some (ScoreForSinkingEnemyShip |> ToBeDoneAtTime hitTime)
        
                        | TooNearBy diff ->
                            None, 
                            Some ((sprintf "%dM SHORT" (diff |> diffToMetres)) |> ToBeDoneAtTime hitTime), 
                            None

                        | TooFarBy diff ->
                            None, 
                            Some ((sprintf "%dM LONG" (diff |> diffToMetres)) |> ToBeDoneAtTime hitTime), 
                            None

                let decorative =
                    match playerShellResult with
                        | DirectHit   -> None
                        | TooNearBy _ -> Some ((NewSplashFlickbookForFiredGun gun hitTime))
                        | TooFarBy _  -> None

                futureHit, futureMessageText, futureScoreChange, decorative

    else
        None, None, None, None

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DebugDrawShipHorizontalHitTestMessage render gunCentreX enemyShips =

    let hitMessage = 
        match gunCentreX |> MappedToUnsinkingEnemyShipIn enemyShips with
            | None       -> "NO SHIP"
            | Some(ship) -> sprintf "%f -> X=%f" gunCentreX ship.CentreX

    Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) MessageY hitMessage

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderSeaBattleScreen render (model:SeaBattleScreenModel) gameTime =

    let DrawBackground () =
        Image1to1 render 0<wu> 0<wu> ImageSeaBattleBackground0.ImageID
        DrawFlickbookInstanceList render model.SkyExplosion gameTime

    let backgroundHeight =
        ImageSeaBattleBackground0.ImageHeight  // They are all the same

    let DrawGun gameTime =
        Gun.DrawGun render backgroundHeight model.GunAim gameTime

    let DrawBasicStuff () =
        DrawBackground ()
        DrawEnemyShips render model.EnemyShips gameTime
        DrawFlickbookInstanceList render model.Decoratives gameTime

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
            ShipsPending     = 0u
            ShipsThrough     = model.ShipsRemaining
            Tanks            = model.ShipsRemaining |> ToTankCountFromShipCount
            Damage           = 0u
            Ammunition       = 10u
            Elevation        = model.GunAim.GunElevation      
        }

    Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) MessageY model.MessageText
    ScoreboardArea render (backgroundHeight |> FloatWuToIntWu)
    DrawScorePanel render (backgroundHeight |> FloatWuToIntWu) scorePanel

    // DEBUG:  DebugDrawShipHorizontalHitTestMessage render model.GunAim.GunCentreX model.EnemyShips

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewSeaBattleScreen scoreAndHiScore shipsRemaining gameTime =
    {
        ScoreAndHiScore     = scoreAndHiScore
        ShipsRemaining      = shipsRemaining
        GunAim              = NewGunWithDefaultTraits DoubleBarrelGun InitialPlayerGunPositionX InitialGunElevation GunStepRate gameTime
        AlliedState         = AlliedShipInPlay
        AlliedCountToSink   = EnemyFireTriesCount
        EnemyShips          = DefaultEnemyShipsArrangement
        Decoratives         = []
        SkyExplosion        = []
        PendingIncoming     = [ToBeDoneAtTime gameTime (IncomingChange ())]
        PendingDamage       = []
        PendingHits         = []
        PendingMessageTexts = []
        PendingScoreChanges = []
        MessageText         = ""
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextSeaBattleScreenState oldState input gameTime frameElapsedTime =

    let newModel =
        match oldState.AlliedState with

            | AlliedShipInPlay ->

                if oldState.EnemyShips.IsEmpty then
                    { oldState with AlliedState = WonScreen(gameTime) }
            
                else
                    let gun                = oldState.GunAim
                    let skyExplosion       = oldState.SkyExplosion
                    let scoreAndHiScore    = oldState.ScoreAndHiScore
                    let alliedState        = oldState.AlliedState
                    let alliedCountToSink  = oldState.AlliedCountToSink
                    let enemyShips         = oldState.EnemyShips
                    let futureIncoming     = oldState.PendingIncoming
                    let futureDamage       = oldState.PendingDamage
                    let futureHits         = oldState.PendingHits
                    let futureMessageTexts = oldState.PendingMessageTexts
                    let futureScoreChanges = oldState.PendingScoreChanges
                    let decoratives        = oldState.Decoratives
                    let messageText        = oldState.MessageText

                    let skyExplosion =
                        skyExplosion |> WithCompletedFlickbooksRemoved gameTime

                    let decoratives =
                        decoratives |> WithCompletedFlickbooksRemoved gameTime

                    let enemyShips =
                        enemyShips |> WithSunkenEnemyShipsRemoved gameTime

                    let gunBaseY =
                        ImageSeaBattleBackground0.ImageHeight
                
                    let gun =
                        UpdatedGunAimAccordingToInput input gameTime frameElapsedTime gunBaseY gun

                    let struct((enemyShips , decoratives , futureDamage , alliedCountToSink), futureIncoming) =
                        struct((enemyShips , decoratives , futureDamage , alliedCountToSink), futureIncoming)
                            |> AppliedForTime gameTime PossiblyLaunchEnemyFire

                    let struct((alliedState , skyExplosion , alliedCountToSink), futureDamage) =
                        struct((alliedState , skyExplosion , alliedCountToSink), futureDamage)
                            |> AppliedForTime gameTime (fun _ _ gameTime -> 
                                PendingDone (
                                    ShipSinking(gameTime),
                                    [NewSkyExplosionFlickBook gameTime],
                                    EnemyFireTriesCount))

                    let struct(enemyShips, futureHits) =
                        struct(enemyShips, futureHits)
                            |> AppliedForTime gameTime (fun oldEnemyShips {HitShipX=enemyShipX} gameTime -> 
                                PendingDone (oldEnemyShips |> WithEnemyShipSinking gameTime enemyShipX))

                    let struct(messageText, futureMessageTexts) =
                        struct(messageText, futureMessageTexts)
                            |> AppliedForTime gameTime (fun _ messageText _ -> 
                                PendingDone messageText)

                    let struct(scoreAndHiScore, futureScoreChanges) =
                        struct(scoreAndHiScore, futureScoreChanges) 
                            |> AppliedForTime gameTime (fun oldScore scoreDelta _ -> 
                                PendingDone (oldScore |> ScoreIncrementedBy scoreDelta))

                    let gun =
                        gun |> UpdatedGunAimWithCompletedShellsRemoved gameTime 

                    let newFutureHit, newFutureMessageText, newFutureScoreChange, newDecorative =
                        ConsiderStateChangesForWhenPlayerFiresGun input gun enemyShips gameTime

                    let consOption xo xs = // TODO: Do we have this anywhere?
                        match xo with
                            | None -> xs
                            | Some x -> x::xs

                    let futureHits         = futureHits         |> consOption newFutureHit
                    let futureMessageTexts = futureMessageTexts |> consOption newFutureMessageText
                    let futureScoreChanges = futureScoreChanges |> consOption newFutureScoreChange
                    let decoratives        = decoratives        |> consOption newDecorative

                    {
                        ShipsRemaining      = oldState.ShipsRemaining  // we don't decrement until the screen is lost.
                        GunAim              = gun
                        SkyExplosion        = skyExplosion
                        ScoreAndHiScore     = scoreAndHiScore
                        AlliedState         = alliedState
                        AlliedCountToSink   = alliedCountToSink
                        EnemyShips          = enemyShips
                        PendingIncoming     = futureIncoming
                        PendingDamage       = futureDamage
                        PendingHits         = futureHits
                        PendingMessageTexts = futureMessageTexts
                        PendingScoreChanges = futureScoreChanges
                        Decoratives         = decoratives
                        MessageText         = messageText
                    }

            | WonScreen(timeEnded) -> 

                let elapsedSinceEngagement = gameTime - timeEnded
                if elapsedSinceEngagement > PauseTimeWhenEnded then
                    { oldState with AlliedState = AirOrSeaBattleScreenOver }
                else
                    oldState

            | ShipSinking(timeEnded) ->   // TODO: This might be in the wrong place because it pauses animations

                let elapsedSinceEngagement = gameTime - timeEnded
                if elapsedSinceEngagement > PauseTimeWhenEnded then
                    let numShips = oldState.ShipsRemaining
                    if numShips > 1u then
                        {
                            oldState with 
                                AlliedState = AlliedShipInPlay
                                ShipsRemaining = numShips - 1u
                        }
                    else
                        { 
                            oldState with 
                                AlliedState = AirOrSeaBattleScreenOver 
                                ShipsRemaining = 0u
                        }
                else
                    oldState

            | AirOrSeaBattleScreenOver ->

                oldState   // Ideology:  Never risk the logic rest of the logic when the screen is over.

    match newModel.AlliedState with
        
        | AirOrSeaBattleScreenOver ->
            if newModel.ShipsRemaining > 0u then
                GoToNextChapter2(newModel)
            else
                GameOver2(newModel)
        
        | _ ->
            StayOnThisChapter2(newModel)

