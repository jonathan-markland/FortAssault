module ScreenSecretPassage

// TODO: I have changed my mind about the "percentage" approach to specifying areas.
// TODO: Could we use overlapping rectangles to model the coastlines roughly?  Seems a shame to lose the work, but it might be simpler.

open Angle
open Time
open Rules
open SharedDrawing
open DrawingCommandsEx
open ScoreHiScore
open Geometry
open Mechanics
open ImagesAndFonts
open InputEventData
open ScorePanel
open FlickBook
open StoryboardChapters
open ImagesAndFonts

let TorpedoTriggerDistance      =    4.0F<wu>
let MineTriggerDistance         =    4.0F<wu>
let ScoreForGettingAShipThrough = 3000u
let PassageSuccessBarrierY      =   10.0F<wu>
let PauseTime                   =    3.0F<seconds>
let ExplosionDuration           =   0.75F<seconds>

let ExplosionFlickBookType = 
    {
        FlickBookDuration       = ExplosionDuration
        FlickBookImages         = [| ImageShipExplode0 ; ImageShipExplode1 ; ImageShipExplode2 ; ImageShipExplode3 |]
        VisibilityBeforeStart   = Hidden
        VisibilityAfterEnd      = Hidden
    }

type SecretPassageScreenFleetStats =
    {
        ShipsStillToNavigate : uint32
        ShipsSuccess         : uint32
        Score                : ScoreAndHiScore
    }

type ShipRotation =
    | FacingLeft
    | Facing1
    | Facing2
    | Facing3
    | FacingUp

type ShipSpeed =
    | ShipStationary
    | ShipSlow
    | ShipMedium
    | ShipFast

type Ship =
    {
        ShipRotation : ShipRotation
        ShipSpeed    : ShipSpeed
        ShipCentre   : PointF32
    }

type ShipState =
    | ShipInPlay     of Ship
    | ShipGotThrough of finishTime : float32<seconds> * Ship
    | ShipExploding  of finishTime : float32<seconds>
    | SecretPassageScreenOver

type Mine =
    {
        MineLocation : PointF32
    }

type Torpedo =
    {
        TorpedoLaunchDetail   :  (float * float) * (float * float) * float32<seconds> * float32<seconds>
        TorpedoPositionGetter :  (float32<seconds> -> MOMReason)
        TorpedoIsHorizontal   :  bool
    }

type SecretPassageScreenModel =
    {
        FleetStats   : SecretPassageScreenFleetStats
        LiveTorpedos : Torpedo list
        Mines        : Mine list
        Animations   : FlickBookInstance list
        Ship         : ShipState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RotatedLeftOnce shipRotation =
    match shipRotation with
        | FacingLeft -> FacingLeft
        | Facing1    -> FacingLeft
        | Facing2    -> Facing1   
        | Facing3    -> Facing2   
        | FacingUp   -> Facing3   

let RotatedRightOnce shipRotation =
    match shipRotation with
        | FacingLeft -> Facing1   
        | Facing1    -> Facing2   
        | Facing2    -> Facing3   
        | Facing3    -> FacingUp  
        | FacingUp   -> FacingUp  

let NewShipRotationAccordingToInput oldShipRotation input =

    if input.Left.JustDown then
        oldShipRotation |> RotatedLeftOnce

    elif input.Right.JustDown then
        oldShipRotation |> RotatedRightOnce

    else
        oldShipRotation

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let SpedUpOnce shipSpeed =

    match shipSpeed with
        | ShipStationary -> ShipSlow      
        | ShipSlow       -> ShipMedium    
        | ShipMedium     -> ShipFast      
        | ShipFast       -> ShipFast      

let SlowedDownOnce shipSpeed =

    match shipSpeed with
        | ShipStationary -> ShipStationary
        | ShipSlow       -> ShipStationary
        | ShipMedium     -> ShipSlow      
        | ShipFast       -> ShipMedium    

let NewShipSpeedAccordingToInput oldShipSpeed input =

    if input.Up.JustDown then
        oldShipSpeed |> SpedUpOnce

    elif input.Down.JustDown then
        oldShipSpeed |> SlowedDownOnce

    else
        oldShipSpeed

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let SpeedInWorldUnits shipSpeed =

    match shipSpeed with 
        | ShipStationary -> 0.0F<wu>
        | ShipSlow       -> 0.2F<wu>
        | ShipMedium     -> 0.4F<wu>
        | ShipFast       -> 0.6F<wu>

let Cosine shipRotation =

    match shipRotation with 
        | FacingLeft -> 1.0F
        | Facing1    -> 0.923F
        | Facing2    -> 0.707F
        | Facing3    -> 0.383F
        | FacingUp   -> 0.0F

let Sine shipRotation =

    match shipRotation with 
        | FacingLeft -> 0.0F
        | Facing1    -> 0.383F
        | Facing2    -> 0.707F
        | Facing3    -> 0.923F
        | FacingUp   -> 1.0F

let NewShipLocation oldShipLocation rotation speed =

    let {xwf=oldx ; ywf=oldy} = oldShipLocation

    let speed = 
        speed |> SpeedInWorldUnits

    let result =
        {
            xwf = oldx + Cosine rotation * -speed
            ywf = oldy - Sine rotation   * speed
        }

    result

let NewShipState oldShipState input =

    match oldShipState with

        | ShipInPlay(ship) ->
            ShipInPlay(
                {
                    ShipCentre   = NewShipLocation ship.ShipCentre ship.ShipRotation ship.ShipSpeed
                    ShipSpeed    = NewShipSpeedAccordingToInput ship.ShipSpeed input
                    ShipRotation = NewShipRotationAccordingToInput ship.ShipRotation input
                })

        | ShipExploding(_) -> 
            oldShipState

        | ShipGotThrough(_) -> 
            oldShipState

        | SecretPassageScreenOver -> 
            oldShipState

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let MinePositionPercentages =
    [
        0.200, 0.352
        0.404, 0.338
        0.556, 0.196
        0.294, 0.606
        0.700, 0.336
        0.264, 0.808
        0.564, 0.688
        0.812, 0.512
        0.908, 0.662
        0.870, 0.832
    ]

let ToMineLocation (xpc,ypc) =

    let { ImageID=_ ; ImageWidth=w ; ImageHeight=h } = ImageSecretPassage
    {
        MineLocation = 
            {
                xwf = float32 xpc * w
                ywf = float32 ypc * h
            }
    }

let DefaultMines () =

    MinePositionPercentages |> List.map ToMineLocation

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let TorpedoFiringPositions = 
    [
        // If the Y coordinates are identical, these are 
        // shown as horizontal torpedos, else diagonal.

        // start pos          end pos          duration       re-fire pause
        // -----------     --------------    -------------    ------------

        // Firing origin positions left bank

        (0.086, 0.214),    (0.318, 0.082),   5.0F<seconds>,   1.0F<seconds>
        (0.086, 0.214),    (0.616, 0.214),   4.1F<seconds>,   2.5F<seconds>
        (0.132, 0.390),    (0.454, 0.126),   6.2F<seconds>,   2.0F<seconds>
        (0.132, 0.390),    (0.826, 0.390),   5.3F<seconds>,   3.0F<seconds>
        (0.170, 0.592),    (0.776, 0.312),   7.0F<seconds>,   3.5F<seconds>
        (0.170, 0.592),    (0.958, 0.592),   7.1F<seconds>,   2.0F<seconds>
        (0.206, 0.790),    (0.846, 0.410),   7.2F<seconds>,   3.5F<seconds>
        (0.206, 0.790),    (0.984, 0.790),   8.3F<seconds>,   1.0F<seconds>
        
        // Firing origin positions right bank
        
        (0.382, 0.102),    (0.138, 0.336),   6.0F<seconds>,   1.0F<seconds>
        (0.698, 0.204),    (0.078, 0.204),   6.4F<seconds>,   2.5F<seconds>
        (0.698, 0.204),    (0.152, 0.490),   6.5F<seconds>,   2.5F<seconds>
        (0.884, 0.454),    (0.130, 0.454),   7.6F<seconds>,   1.5F<seconds>
        (0.962, 0.608),    (0.260, 0.914),   7.7F<seconds>,   3.0F<seconds>
        (0.962, 0.608),    (0.178, 0.608),   7.8F<seconds>,   1.0F<seconds>
    ]

let NewTorpedoForFiringPosition launchDetail gameTime = 

    let ((sx,sy), (ex,ey), duration, refirePause) = launchDetail

    let { ImageID=_ ; ImageWidth=w ; ImageHeight=h } = ImageSecretPassage

    let sx' = float32 sx * w
    let ex' = float32 ex * w
    let sy' = float32 sy * h
    let ey' = float32 ey * h

    let t = gameTime + refirePause

    {
        TorpedoLaunchDetail = 
            launchDetail

        TorpedoPositionGetter =
            FunctionThatGetsPositionOfMovingObject
                LinearMotion {xwf=sx'; ywf=sy'} {xwf=ex'; ywf=ey'} t duration

        TorpedoIsHorizontal = 
            (sy' = ey')
    }

let DefaultTorpedos gameTime = 
    
    TorpedoFiringPositions 
        |> List.mapi (fun i pos -> 
            NewTorpedoForFiringPosition pos gameTime)

let NewFutureTorpedoBasedOn (oldTorpedo:Torpedo) gameTime =

    NewTorpedoForFiringPosition oldTorpedo.TorpedoLaunchDetail gameTime


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DefaultShipLocation =

    let { ImageID=_ ; ImageWidth=w ; ImageHeight=h } = ImageSecretPassage

    {
        ShipRotation = FacingLeft
        ShipSpeed    = ShipStationary
        ShipCentre   = { xwf = 0.886F * w ; ywf = 0.938F * h }
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let FoldAllShipTriggerPoints shipCentre direction f state =

    let length = 10.0F<wu>  // TODO: This will have to be a constant for now, but should be based on half the rough length of the ship

    let cos = Cosine direction
    let sin = Sine direction

    let calcTriggerPoint p (factor:float32<wu>) = 
        let {xwf=x ; ywf=y} = p
        { xwf = x+cos*factor ; ywf = y+sin*factor }

    let state = f state (calcTriggerPoint shipCentre (length * 1.0F))
    let state = f state shipCentre
    let state = f state (calcTriggerPoint shipCentre (length * -1.0F))
    state

let ShipIsWithinRegionOf ship adversaryPosition triggerDistance =

    let test (state:bool) pointOnShip =
        state || pointOnShip |> IsWithinRegionOf adversaryPosition triggerDistance 

    FoldAllShipTriggerPoints ship.ShipCentre ship.ShipRotation test false

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewExplosion centreLocation gameTime =
    {
        FlickBookType            = ExplosionFlickBookType
        FlickBookStartTime       = gameTime
        FlickBookMechanicsObject = MechanicsControlledStationaryObject centreLocation gameTime ExplosionDuration
    }

let Impact location shipState animationList gameTime =

    match location with

        | Some(explosionLocation) ->
            ((NewExplosion explosionLocation gameTime)::animationList, (ShipExploding(gameTime)))

        | None ->
            (animationList, shipState)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ShipVersusTorpedos torpedos explosions shipState gameTime =

    match shipState with
        
        | ShipExploding(_)
        | ShipGotThrough(_)
        | SecretPassageScreenOver -> 

            torpedos, explosions, shipState

        | ShipInPlay(ship) ->

            let isShipHitByTorpedo ship torpedo =
                match torpedo.TorpedoPositionGetter gameTime with
                    | MOMYetToAppear(_) -> None
                    | MOMDisappeared(_) -> None
                    | MOMVisibleAtPosition(torpedoPosition) -> 
                        if ShipIsWithinRegionOf ship torpedoPosition TorpedoTriggerDistance then
                            Some(torpedoPosition)
                        else
                            None

            // Scan torpedos for proximity to ship.  In range -> create explosion and cons to explosions list, out of range -> cons to surviving torpedos list.            

            let folder (newTorpList, animationList, shipState) torpedo =
                match isShipHitByTorpedo ship torpedo with
                    | Some(explosionLocation) -> 
                        ((NewFutureTorpedoBasedOn torpedo gameTime)::newTorpList, (NewExplosion explosionLocation gameTime)::animationList, (ShipExploding(gameTime)) )
                    | None -> 
                        (torpedo::newTorpList, animationList, shipState)

            let survivingTorpedos, updatedExplosions, resultingShip = 
                torpedos |> List.fold folder ([], explosions, shipState)

            survivingTorpedos, updatedExplosions, resultingShip

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ShipVersusMines mines explosions shipState gameTime =

    match shipState with

        | ShipExploding(_)
        | ShipGotThrough(_)
        | SecretPassageScreenOver -> 
        
            explosions, shipState

        | ShipInPlay(ship) ->

            let isShipHitByMine ship {MineLocation=minePosition} =
                if ShipIsWithinRegionOf ship minePosition MineTriggerDistance then
                    Some(minePosition)
                else
                    None

            // Scan mines for proximity to ship.  In range -> create explosion and cons to explosions list, out of range -> cons to surviving torpedos list.            

            let folder (explosionList, shipState) mine =
                Impact (isShipHitByMine ship mine) shipState explosionList gameTime

            let updatedExplosions, resultingShip = 
                mines |> List.fold folder (explosions, shipState)

            updatedExplosions, resultingShip

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let LeftCoast =
    (0.0F, 
        [
            (0.022F, 0.000F)
            (0.132F, 0.380F)
            (0.222F, 0.890F)
            (0.320F, 1.000F)
        ])

let RightCoast = 
    (1.0F,
        [
            (0.240F, 0.000F)
            (0.336F, 0.116F)
            (0.696F, 0.200F)
            (1.000F, 0.726F)
            (1.000F, 1.000F)
        ])

let CoastXAt (defaultCoastX, coastLines) (y:float32<wu>) =

    let { ImageID=_ ; ImageWidth=w ; ImageHeight=h } = ImageSecretPassage   // TODO: Bit fed up of de-structuring this all over this listing.

    let ypc = y / h

    let findIn sourceList defaultIfNotFound =
        let findResult = List.pairwise sourceList |> List.tryPick (fun ((x1,y1),(x2,y2)) -> 
            if ypc < y1 then None
            else 
                if ypc > y2 then None
                else
                    Some(InterpolateLineSegment y1 x1 y2 x2 ypc))
        findResult |> Option.defaultValue defaultIfNotFound

    let left    = findIn coastLines defaultCoastX
    let leftPos = {xwf=left * w  ; ywf=y}
    leftPos

let CoastLeftAndRightAtY (y:float32<wu>) =

    (CoastXAt LeftCoast y, CoastXAt RightCoast y)

let ShipBeyondBound ship inequality coastLine =

    let folder state pointOnShip =
        match state with
            | Some(_) ->
                state
            | None ->
                let coastX = CoastXAt coastLine pointOnShip.ywf
                if inequality pointOnShip coastX then Some(pointOnShip) else None

    FoldAllShipTriggerPoints ship.ShipCentre ship.ShipRotation folder None

let ShipVersusCoastline explosions shipState gameTime =

    match shipState with

        | ShipExploding(_)
        | ShipGotThrough(_)
        | SecretPassageScreenOver -> 
        
            explosions, shipState

        | ShipInPlay(ship) ->

            let hitL = ShipBeyondBound ship PointToLeftOf LeftCoast
            let hitR = ShipBeyondBound ship PointToRightOf RightCoast

            match hitL |> Option.orElse hitR with
                | None ->
                    explosions, shipState
                | Some(explosionLocation) -> 
                    ((NewExplosion explosionLocation gameTime)::explosions, (ShipExploding(gameTime)))

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let PositionHasGotThrough {xwf=_ ; ywf=y} =

    y < PassageSuccessBarrierY

let HasShipCompletedSuccessfully shipState gameTime =

    match shipState with

        | ShipExploding(_)
        | ShipGotThrough(_)
        | SecretPassageScreenOver -> 
        
            shipState

        | ShipInPlay(ship) ->

            if ship.ShipCentre |> PositionHasGotThrough then
                ShipGotThrough (gameTime, ship)
            else
                shipState

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let WithCompletedTorpedosRemovedAndReplaced gameTime torpedos =

    torpedos |> List.map (fun oldTorpedo ->
        match oldTorpedo.TorpedoPositionGetter gameTime with
            | MOMDisappeared(_) ->
                NewFutureTorpedoBasedOn oldTorpedo gameTime
            | _ -> 
                oldTorpedo)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ShipImageFor direction =

    match direction with 
        | FacingLeft -> ImageShip0
        | Facing1    -> ImageShip1
        | Facing2    -> ImageShip2
        | Facing3    -> ImageShip3
        | FacingUp   -> ImageShip4

let RenderSecretPassageScreen render gameTime secretPassageScreenModel =

    let DrawMines mines =
        mines |> List.iter (fun {MineLocation=loc} ->
            CentreImage render loc.xwf loc.ywf ImageMine
            // SquareAroundPoint render loc.xwf loc.ywf (MineTriggerDistance * 2.0F) 0xFF0000u
            )
    
    let DrawTorpedos torpedos =
        torpedos |> List.iter (fun {TorpedoPositionGetter=getTorpedoPosition ; TorpedoIsHorizontal=isHoriz} ->
            let posToDrawAt = getTorpedoPosition gameTime
            match posToDrawAt with
                | MOMYetToAppear(_) -> ()
                | MOMDisappeared(_) -> ()
                | MOMVisibleAtPosition({xwf=x ; ywf=y}) ->
                    let img = if isHoriz then ImageTorpedo0 else ImageTorpedo22Degrees
                    CentreImage render x y img
                    // Debug:  SquareAroundPoint render x y (TorpedoTriggerDistance * 2.0F) 0xFFFF00u
                    )

    let DrawShip shipState =
        match shipState with

            | SecretPassageScreenOver
            | ShipExploding(_) ->
                ()

            | ShipGotThrough(_,ship)
            | ShipInPlay(ship) ->
                let c = ship.ShipCentre
                CentreImage render c.xwf c.ywf (ShipImageFor ship.ShipRotation) 
                // Debug:  FoldAllShipTriggerPoints c ship.ShipRotation (fun a point -> SquareAroundPoint render point.xwf point.ywf 2.0F<wu> 0xFFFFFFu) ()

    // Debug:  let DrawCoastSensitiveRegions () =
    // Debug:      for i in 0..159 do // TODO: hack 159
    // Debug:          let leftPos, rightPos = CoastLeftAndRightAtY (LanguagePrimitives.Float32WithMeasure<wu>(float32 i))
    // Debug:          SquareAroundPoint render leftPos.xwf leftPos.ywf 1.0F<wu> 0x000000u
    // Debug:          SquareAroundPoint render rightPos.xwf rightPos.ywf 1.0F<wu> 0x000000u

    let h = ImageSecretPassage.ImageHeight

    Image1to1 render 0<wu> 0<wu> ImageSecretPassage.ImageID
    ScoreboardArea render (h |> FloatWuToIntWu)
    DrawMines      secretPassageScreenModel.Mines
    DrawTorpedos   secretPassageScreenModel.LiveTorpedos
    DrawShip       secretPassageScreenModel.Ship
    DrawFlickbookInstanceList render secretPassageScreenModel.Animations gameTime
    // Debug:  DrawCoastSensitiveRegions ()

    let scorePanel =
        {
            ScoreAndHiScore  = secretPassageScreenModel.FleetStats.Score
            ShipsPending     = secretPassageScreenModel.FleetStats.ShipsStillToNavigate
            ShipsThrough     = secretPassageScreenModel.FleetStats.ShipsSuccess
            Tanks            = secretPassageScreenModel.FleetStats.ShipsSuccess |> ToTankCountFromShipCount
            Damage           = 0u
            Ammunition       = 10u
            Elevation        = 0.0F<degrees>
        }

    DrawScorePanel render (h |> FloatWuToIntWu) scorePanel

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewSecretPassageScreen score initialShipCount screenStartGameTime =
    {
        FleetStats = 
            {
                ShipsStillToNavigate = initialShipCount
                ShipsSuccess = 0u
                Score = score
            }
        LiveTorpedos  = DefaultTorpedos screenStartGameTime
        Ship          = ShipInPlay(DefaultShipLocation)
        Animations    = []
        Mines         = DefaultMines ()
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RoundCompleted ship gameTime =

    match ship with
        
        | SecretPassageScreenOver
        | ShipInPlay(_) ->
            false

        | ShipExploding(finishTime)
        | ShipGotThrough(finishTime,_) -> 
            gameTime > finishTime + PauseTime

let CheckIfRoundComplete stats ship gameTime =

    let statsForNewRound stats newShipsSuccess newScore =

        let stats = 
            {
                ShipsStillToNavigate = stats.ShipsStillToNavigate - 1u
                ShipsSuccess         = newShipsSuccess
                Score                = newScore
            }

        let ship = 
            if stats.ShipsStillToNavigate = 0u then SecretPassageScreenOver else ShipInPlay(DefaultShipLocation)

        (stats, ship)

    if RoundCompleted ship gameTime then
        match ship with

            | ShipInPlay(_) -> 
                failwith "should not happen"

            | SecretPassageScreenOver -> 
                failwith "should not happen"

            | ShipExploding(_) ->
                statsForNewRound stats stats.ShipsSuccess stats.Score

            | ShipGotThrough(_) ->
                statsForNewRound stats (stats.ShipsSuccess + 1u) (stats.Score |> ScoreIncrementedBy ScoreForGettingAShipThrough)

    else
        (stats, ship)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextSecretPassageScreenState oldState input gameTime =

    let ship = oldState.Ship

    let newModel =
        match ship with
            | SecretPassageScreenOver ->
                oldState   // Ideology:  Never risk the logic below if level is supposed to be over already.

            | _ ->

                let stats      = oldState.FleetStats
                let torpedos   = oldState.LiveTorpedos
                let animations = oldState.Animations
                let mines      = oldState.Mines

                let ship =
                    NewShipState ship input

                let torpedos =
                    torpedos |> WithCompletedTorpedosRemovedAndReplaced gameTime

                let animations =
                    animations |> WithCompletedFlickbooksRemoved gameTime

                let torpedos, animations, ship =
                    ShipVersusTorpedos torpedos animations ship gameTime
    
                let animations, ship =
                    ShipVersusMines mines animations ship gameTime

                let animations, ship =
                    ShipVersusCoastline animations ship gameTime

                let ship =
                    HasShipCompletedSuccessfully ship gameTime

                let stats, ship =
                    CheckIfRoundComplete stats ship gameTime

                {
                    FleetStats   = stats
                    LiveTorpedos = torpedos
                    Ship         = ship
                    Animations   = animations
                    Mines        = mines
                }
    
    match newModel.Ship with

        | SecretPassageScreenOver -> 
            if newModel.FleetStats.ShipsSuccess > 0u then
                GoToNextChapter2(newModel)
            else
                GameOver2(newModel)

        | _ -> 
            StayOnThisChapter2(newModel)

