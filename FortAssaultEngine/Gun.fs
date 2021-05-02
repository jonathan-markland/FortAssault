module Gun

// TODO:  We're not using the Ammunition display at the moment, and I think it should be included in this model here.

open Angle
open Time
open Geometry
open Input
open InputEventData
open DrawingFunctions
open ResourceIDs
open Mechanics
open ImagesAndFonts
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type Shell =
    {
        ShellMechanicsObject : MechanicsObjectModel
        ShellStartTime       : GameTime  // TODO: Delete this an use the one inside the ShellMechanicsObject
    }

type GunType = SingleBarrelGun | DoubleBarrelGun

type GunTraits =
    {
        GunType              : GunType
        GunLeftExtent        : float32<epx>
        GunRightExtent       : float32<epx>
        GunStepRatePerSecond : float<epx/seconds>
        HighestShellY        : float32<epx>
        GunRepeatTime        : GameTime  // TODO: Turn into a parameter of GunAim within GunType field?
        GunShellDuration     : GameTime  // TODO: More realistic in sea battle to relate this to elevation.
        GunLowestElevation   : float<degrees>
        GunHighestElevation  : float<degrees>
        GunElevationStepRate : float<degrees/seconds>
    }

type GunAim =
    {
        GunTraits           : GunTraits
        GunCentreX          : float32<epx>
        GunElevation        : float<degrees>
        Shells              : Shell list
        LastFireTime        : GameTime
    }

let DefaultGunTraits gunType stepRate =
    {
        GunType              = gunType
        GunLeftExtent        =  20.0F<epx>
        GunRightExtent       = 300.0F<epx>
        GunStepRatePerSecond =  80.0<epx/seconds>
        HighestShellY        =   0.0F<epx>
        GunRepeatTime        =   0.5<seconds>   // TODO: Turn into a parameter of GunAim within GunType field?
        GunShellDuration     =   0.7<seconds>   // TODO: More realistic in sea battle to relate this to elevation.
        GunLowestElevation   =  5.0<degrees>
        GunHighestElevation  = 80.0<degrees>
        GunElevationStepRate = stepRate
    }

let NewGunWithDefaultTraits gunType gunCentreX gunElevationTenths stepRate gameTime =

    let gunTraits = DefaultGunTraits gunType stepRate

    { 
        GunTraits          = gunTraits
        GunCentreX         = gunCentreX
        GunElevation       = gunElevationTenths
        Shells             = []
        LastFireTime       = gameTime - gunTraits.GunRepeatTime
    }


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DoubleBarrelledGunXPositions x =

    let separation = ShipGunNozzleImageWidthF * 0.6F
    struct (x - separation, x + separation)
    
let GetGunImageLocation gunBaseY gunAim =

    let elevationAngle = (float32 gunAim.GunElevation      ) / 10.0F
    let nozzleHeight   = LanguagePrimitives.Float32WithMeasure<epx>( float32(int ( 9.5F + 0.152F * elevationAngle)) )
    let stemHeight     = LanguagePrimitives.Float32WithMeasure<epx>( float32(int (15.5F + 0.104F * elevationAngle)) )
    let stemTopY = (gunBaseY - stemHeight)
    let nozzleTopY = (gunBaseY - (stemHeight + nozzleHeight))

    struct (stemHeight, nozzleHeight, stemTopY, nozzleTopY)

let ShellPercentageCompleted gunTraits shell gameTime =

    let timeOffset = gameTime - shell.ShellStartTime
    timeOffset / gunTraits.GunShellDuration

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawGun render gunBaseY gunAim gameTime =

    let drawShell shell =
        match shell.ShellMechanicsObject.PositionGetter gameTime with

            | MOMVisibleAtPosition({ptx=x ; pty=y}) ->
                let v = (1.0 - ShellPercentageCompleted gunAim.GunTraits shell gameTime)
                let w = (v * 12.0<epx> + 4.0<epx>) |> EpxF64toF32  // TODO: base on ShipGunBulletImageWidth
                let h = (v * 5.0<epx> + 1.0<epx> ) |> EpxF64toF32
                let x = x - (w / 2.0F)
                let y = y - (h / 2.0F)
                ImageStretched render x y (ShipGunBulletImageID |> ImageFromID) (w |> RoundF32EpxToIntEpx) (h |> RoundF32EpxToIntEpx)

            | _ -> ()

    let drawGun (centreX:float32<epx>) =
        let struct (stemHeight, nozzleHeight, stemTopY, nozzleTopY) = GetGunImageLocation gunBaseY gunAim
        let stemLeftX   = centreX - ((ShipGunStemImageWidth / 2) |> IntToF32Epx)
        let nozzleLeftX = centreX - ((ShipGunNozzleImageWidth / 2) |> IntToF32Epx)

        ImageStretched 
            render 
            stemLeftX stemTopY 
            (ShipGunStemImageID |> ImageFromID) 
            ShipGunStemImageWidth (stemHeight |> RoundF32EpxToIntEpx)

        ImageStretched 
            render 
            nozzleLeftX nozzleTopY 
            (ShipGunNozzleImageID |> ImageFromID) 
            ShipGunNozzleImageWidth (nozzleHeight |> RoundF32EpxToIntEpx)

    match gunAim.GunTraits.GunType with

        | SingleBarrelGun ->
            drawGun gunAim.GunCentreX

        | DoubleBarrelGun ->
            let struct (leftGunX, rightGunX) = DoubleBarrelledGunXPositions gunAim.GunCentreX
            drawGun leftGunX
            drawGun rightGunX

    gunAim.Shells |> List.iter drawShell
    // For debug:   CentreImage render gunAim.GunCentreX gunBaseY ImageFinalBossActiveTarget

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let TopmostScreenPositionOfShellByElevation gunTraits nozzleTopY elevationTenths =

    let e = float32(gunTraits.GunHighestElevation - elevationTenths)
    let screenRange = nozzleTopY - gunTraits.HighestShellY
    let mult = screenRange / float32 gunTraits.GunHighestElevation
    gunTraits.HighestShellY + e * mult

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewShell gunCentreX gunBaseY gunAim gameTime =

    let struct (_stemHeight, _nozzleHeight, _stemTopY, nozzleTopY) = GetGunImageLocation gunBaseY gunAim
    
    let x = gunCentreX

    let highestY =
        TopmostScreenPositionOfShellByElevation gunAim.GunTraits nozzleTopY gunAim.GunElevation

    let shellStartPosition = { ptx=x ; pty=nozzleTopY }
    let shellEndPosition   = { ptx=x ; pty=highestY }

    {
        ShellStartTime = gameTime
        ShellMechanicsObject =
            MechanicsControlledMovingObject 
                SlowingDownMotion
                shellStartPosition
                shellEndPosition
                gameTime
                gunAim.GunTraits.GunShellDuration
    }
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// WARNING:  Completed shells are NOT removed, thus allowing completed shells
///           to be identified (and reported at their final point) by the caller.
let UpdatedGunAimAccordingToInput input (gameTime:GameTime) (frameElapsedTime:GameTime) gunBaseY oldGun =

    let gunTraits = oldGun.GunTraits

    let e = oldGun.GunElevation
    let x = oldGun.GunCentreX
    let shells = oldGun.Shells
    let lastFireTime = oldGun.LastFireTime

    let e' =
        if input.Up.Held && input.Down.Held then
            e
        elif input.Up.Held then
            max gunTraits.GunLowestElevation (e - gunTraits.GunElevationStepRate * frameElapsedTime)
        elif input.Down.Held then
            min gunTraits.GunHighestElevation (e + gunTraits.GunElevationStepRate * frameElapsedTime)
        else
            e

    let x' = 
        if input.Left.Held && input.Right.Held then
            x
        elif input.Left.Held then
            let a = (gunTraits.GunStepRatePerSecond * frameElapsedTime) |> EpxF64toF32
            max gunTraits.GunLeftExtent (x - a)
        elif input.Right.Held then
            let a = (gunTraits.GunStepRatePerSecond * frameElapsedTime) |> EpxF64toF32
            min gunTraits.GunRightExtent (x + a)
        else
            x

    let shells, lastFireTime =

        let elapsedTime = gameTime - oldGun.LastFireTime

        if input.Fire.JustDown && elapsedTime > gunTraits.GunRepeatTime then
            
            let shellAtX centreX = NewShell centreX gunBaseY oldGun gameTime

            match gunTraits.GunType with

                | SingleBarrelGun -> 
                    let newShell = shellAtX oldGun.GunCentreX
                    (newShell::shells, gameTime)

                | DoubleBarrelGun -> 
                    let struct (leftGunX, rightGunX) = DoubleBarrelledGunXPositions oldGun.GunCentreX
                    ((shellAtX leftGunX)::(shellAtX rightGunX)::shells, gameTime)
        else
            shells, lastFireTime

    { oldGun with GunCentreX = x' ; GunElevation = e' ; Shells = shells ; LastFireTime = lastFireTime }
        

/// Returns a new gun with completed shells removed.
let UpdatedGunAimWithCompletedShellsRemoved gameTime oldGun =

    let shells =
        oldGun.Shells |> ListWithCompletedMOMsRemoved (fun shell -> shell.ShellMechanicsObject) gameTime

    { oldGun with Shells = shells }
