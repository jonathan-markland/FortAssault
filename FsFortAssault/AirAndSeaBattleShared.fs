module AirAndSeaBattleShared

open Angle
open Time
open Mechanics
open FlickBook
open Geometry
open SharedDrawing
open DrawingCommands
open DrawingCommandsEx
open ImagesAndFonts
open Algorithm
open FontAlignment

let SkyExplosionDuration  = 3.0F<seconds>
let EnemyShipSinkDuration = 6.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type AlliedState =
    | AlliedShipInPlay
    | ShipSinking      of startTime:float32<seconds>
    | WonScreen        of startTime:float32<seconds>
    | AirOrSeaBattleScreenOver

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// TODO:  Issues with constants, and should this be elsewhere -- more general than just this screen?

let ReflectedElevation elevation =

    if elevation <= 45.0F<degrees> then elevation else 90.0F<degrees> - elevation

let ElevationToScreenY elevation =

    let angle                   = elevation |> ReflectedElevation
    let angleInReverseUnitSpace = angle |> RangeMap 0.0F<degrees> 45.0F<degrees> 1.0F 0.0F
    let asSquared               = angleInReverseUnitSpace |> Squared
    let inScreenSpace           = asSquared |> RangeMap 0.0F 1.0F 100.0F<epx> 148.0F<epx>
    let result                  = inScreenSpace + 0.5F<epx>
    result

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type EnemyShip =
    {
        CentreX        : float32<epx>
        BaseY          : float32<epx>
        ShipImage      : ImageWithDimensions
        SinkStartTime  : float32<seconds> option
        ElevationToHit : float32<degrees>
    }

let NewEnemyShip centreX shipImage elevationToHit =
    {
        CentreX        = centreX
        BaseY          = elevationToHit |> ElevationToScreenY
        ShipImage      = shipImage 
        SinkStartTime  = None
        ElevationToHit = elevationToHit
    }

let DefaultEnemyShipsArrangement =
    [
        NewEnemyShip  29.0F<epx> ImageEnemyShip0 44.0F<degrees>
        NewEnemyShip 143.0F<epx> ImageEnemyShip1 39.5F<degrees>
        NewEnemyShip 198.0F<epx> ImageEnemyShip2 36.5F<degrees>
        NewEnemyShip 248.0F<epx> ImageEnemyShip3 41.0F<degrees>
        NewEnemyShip 288.0F<epx> ImageEnemyShip4 32.5F<degrees>
    ]

let EnemyShipIsLive gameTime enemyShip =

    match enemyShip.SinkStartTime with
        | None ->
            true
        | Some(startTime) ->
            (gameTime - startTime) <= EnemyShipSinkDuration

let WithSunkenEnemyShipsRemoved gameTime enemyShipList =

    enemyShipList |> PlanetSavingListFilter (EnemyShipIsLive gameTime)

let DrawEnemyShips render listOfShips gameTime =

    listOfShips |> List.iter (fun ship -> 

        let sinkLevel =
            match ship.SinkStartTime with
                | None -> 0.0F<epx>
                | Some(sinkStartTime) ->
                    let elapsed = gameTime - sinkStartTime
                    let offset = min elapsed EnemyShipSinkDuration
                    (offset / EnemyShipSinkDuration) * ship.ShipImage.ImageHeight

        let srcLeft   = 0
        let srcTop    = 0
        let srcWidth  = int ship.ShipImage.ImageWidth
        let srcHeight = int ship.ShipImage.ImageHeight - int sinkLevel

        let dstLeft   = ship.CentreX - (ship.ShipImage.ImageWidth / 2.0F)
        let dstTop    = ship.BaseY - (ship.ShipImage.ImageHeight - sinkLevel)
        let dstWidth  = ship.ShipImage.ImageWidth
        let dstHeight = srcHeight |> IntToFloatEpx
        
        let img = ship.ShipImage.ImageID

        let cmd = 
            DrawSubImageStretchedToTarget(  // TODO: This broke the system with a struct-DU
                srcLeft, srcTop, srcWidth, srcHeight, 
                dstLeft, dstTop, dstWidth, dstHeight, 
                img)

        render cmd
    )

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let SkyExplosionFlickBookType = 
    {
        FlickBookDuration   = SkyExplosionDuration
        FlickBookImages     = 
            [| 
                ImageSeaBattleBackground1
                ImageSeaBattleBackground2
                ImageSeaBattleBackground2
                ImageSeaBattleBackground3
                ImageSeaBattleBackground3
            |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Hidden
    }

let NewSkyExplosionFlickBook gameTime =

    let w = ImageSeaBattleBackground0.ImageWidth   // They are all the same
    let h = ImageSeaBattleBackground0.ImageHeight  // They are all the same

    {
        FlickBookType            = SkyExplosionFlickBookType
        FlickBookMechanicsObject = MechanicsControlledStationaryObject { ptx = w / 2.0F ; pty = h / 2.0F } gameTime SkyExplosionDuration
        FlickBookStartTime       = gameTime
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawStageCompleteMessage render =
    Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) 75<epx> "STAGE COMPLETE"

let DrawShipDestroyedMessage render =
    Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) 75<epx> "SHIP DESTROYED"

