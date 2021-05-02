module AirAndSeaBattleShared

open Angle
open Time
open Mechanics
open FlickBook
open Geometry
open SharedDrawing
open DrawingShapes
open DrawingFunctions
open ResourceIDs
open Algorithm
open ImagesAndFonts
open StaticResourceAccess

let SkyExplosionDuration  = 3.0<seconds>
let EnemyShipSinkDuration = 6.0<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type AlliedState =
    | AlliedShipInPlay
    | ShipSinking      of startTime:GameTime
    | WonScreen        of startTime:GameTime
    | AirOrSeaBattleScreenOver

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// TODO:  Issues with constants, and should this be elsewhere -- more general than just this screen?

let ReflectedElevation elevation =

    if elevation <= 45.0<degrees> then elevation else 90.0<degrees> - elevation

let ElevationToScreenY elevation =

    let angle                   = elevation |> ReflectedElevation
    let angleInReverseUnitSpace = angle |> RangeMap 0.0<degrees> 45.0<degrees> 1.0 0.0
    let asSquared               = angleInReverseUnitSpace |> Squared |> (float32)
    let inScreenSpace           = asSquared |> RangeMap 0.0F 1.0F 100.0F<epx> 148.0F<epx>
    let result                  = inScreenSpace + 0.5F<epx>
    result

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type EnemyShip =
    {
        CentreX        : float32<epx>
        BaseY          : float32<epx>
        ShipImage      : Image
        SinkStartTime  : GameTime option
        ElevationToHit : float<degrees>
    }

let NewEnemyShip centreX shipImage elevationToHit =
    {
        CentreX        = centreX
        BaseY          = elevationToHit |> ElevationToScreenY
        ShipImage      = shipImage 
        SinkStartTime  = None
        ElevationToHit = elevationToHit
    }

#if SHORT_PLAYTHROUGH

let DefaultEnemyShipsArrangement () =
    [
        NewEnemyShip 100.0F<epx> (ImageEnemyShip4 |> ImageFromID) 40.0F<degrees>
        NewEnemyShip 220.0F<epx> (ImageEnemyShip3 |> ImageFromID) 40.0F<degrees>
    ]

#else

let DefaultEnemyShipsArrangement () =
    [
        NewEnemyShip  29.0F<epx> (ImageEnemyShip0 |> ImageFromID) 44.0<degrees>
        NewEnemyShip 143.0F<epx> (ImageEnemyShip1 |> ImageFromID) 39.5<degrees>
        NewEnemyShip 198.0F<epx> (ImageEnemyShip2 |> ImageFromID) 36.5<degrees>
        NewEnemyShip 248.0F<epx> (ImageEnemyShip3 |> ImageFromID) 41.0<degrees>
        NewEnemyShip 288.0F<epx> (ImageEnemyShip4 |> ImageFromID) 32.5<degrees>
    ]

#endif

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

        let shipHeight = ship.ShipImage.ImageMetadata.ImageHeight
        let shipWidth  = ship.ShipImage.ImageMetadata.ImageWidth

        let sinkLevel =
            match ship.SinkStartTime with
                | None -> 0.0F<epx>
                | Some(sinkStartTime) ->
                    let elapsed = gameTime - sinkStartTime
                    let offset = min elapsed EnemyShipSinkDuration
                    let a = float32 (offset / EnemyShipSinkDuration)
                    let b = (shipHeight |> IntToF32Epx)
                    a * b

        let srcLeft   = 0
        let srcTop    = 0
        let srcWidth  = shipWidth |> RemoveEpxFromInt
        let srcHeight = (shipHeight |> RemoveEpxFromInt) - (sinkLevel |> RoundF32EpxToInt)

        let dstLeft   = ship.CentreX - ((shipWidth |> IntToF32Epx) / 2.0F)
        let dstTop    = ship.BaseY - ((shipHeight  |> IntToF32Epx) - sinkLevel)
        let dstWidth  = shipWidth
        let dstHeight = srcHeight |> AsIntEpx
        
        let cmd = 
            DrawSubImageStretchedToTarget(  // TODO: This broke the system with a struct-DU
                srcLeft, srcTop, srcWidth, srcHeight, 
                dstLeft, dstTop, dstWidth, dstHeight, 
                ship.ShipImage)

        render cmd
    )

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let SkyExplosionFlickBookType () =   // TODO: Made into a function to avoid framework static-initialize-order problems in Fable
    {
        FlickBookDuration   = SkyExplosionDuration
        FlickBookImages     = 
            [| 
                ImageSeaBattleBackground1 |> ImageFromID
                ImageSeaBattleBackground2 |> ImageFromID
                ImageSeaBattleBackground2 |> ImageFromID
                ImageSeaBattleBackground3 |> ImageFromID
                ImageSeaBattleBackground3 |> ImageFromID
            |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Hidden
    }

let NewSkyExplosionFlickBook gameTime =

    let imgBack = ImageSeaBattleBackground0 |> ImageFromID

    let w = imgBack.ImageMetadata.ImageWidth  |> IntToF32Epx // They are all the same
    let h = imgBack.ImageMetadata.ImageHeight |> IntToF32Epx // They are all the same

    {
        FlickBookType            = SkyExplosionFlickBookType ()
        FlickBookMechanicsObject = MechanicsControlledStationaryObject { ptx = w / 2.0F ; pty = h / 2.0F } gameTime SkyExplosionDuration
        FlickBookStartTime       = gameTime
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawStageCompleteMessage render =
    Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) 75<epx> "STAGE COMPLETE"

let DrawShipDestroyedMessage render =
    Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) 75<epx> "SHIP DESTROYED"

