module TankMapFileLoader

open DrawingCommands
open ImagesAndFonts
open ListSplicer
open ResourceFileMetadata

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

//  X   Barricade
//  W   Water
//  *   Mines
//  >   Potential enemy tank location
//  T   Tower
//  =   Bridge
//  -   Sand

#if SHORT_PLAYTHROUGH

let TankMapDefinitions =
    [|
        [|
            "WWWWWWWW--WW"
            "WWWW>WW-WW--"
            "==----XX----"
            "---T--------"
            "============"
            "->----------"
            "W------*----"
            "WWW****WW---"
            "WW****WW*WWW"
        |]
    |]

#else

let TankMapDefinitions =
    [|
        [|
            "T----------X-------------WWWW------WW-WW---"
            "X-T--------X-T-T----------WW-----WWW-WWWW--"
            "X----------X------W-------W->------WWW--WW-"
            "X----------X------WW-T->--W----------------"
            "X------X---X---T--WWW----------------------"
            "->-WWW-X---X-----WWWW-------->-----WW------"
            "---->--X--------WWWW--------------WWW------"
            "--T-T--X------>--------T--XX-----WWW---WWWW"
            "T------X-----------------XXXX-----------WWW"
        |]
        [|
            "WW-------X-----WWWWW-*---X---WWWWWWWWW-X"
            "W--XXXXXXXXXX---W-W-W*->-X----W-WWW-W--X"
            "W---------->-------=======----------T--X"
            "WW---XXXXXXXX----X--**>--X--X----X->X---"
            "==--X--=====--->-X--*-------X-------X--X"
            "WW-X-----X->-----X-----**X>-X----X-----X"
            "==------X>----------***--X-------X--T--X"
            "W-T------X--->----WW*-->-X------XXX----X"
            "W-XXXX---X------XXXX*----X-------X-----X"
        |]
        [|
            "XXX--XX--XXXXXXXXXXXXX----X-WW---T-------WWWW"
            "--X-X--XX--WWW--WWWW------X-W--T---*----WWWW-"
            "---X--------WWWWW---------X-------***----W>W-"
            "----->--------WWWWW-------X---XXX*****---WWWW"
            "->-----------WWWWWW---X>--XT----->-------===="
            "----->------>-W====---X-------XXX*****---WWWW"
            "---X------>-=====WWW--X>----------***--->===="
            "--X-X--XX------WW-----X--------T---*-----WWW-"
            "XXX--XX--XXXXXXXXXXXXXX---WWW----T-----WWWWW-"
        |]
        [|
            "-WWWWW-X--X***--T-----X----WWWWWWWWWWWW--WW"
            "W-WWW--X>-X***---WWT--X----WWWW-WWWWWW-WW--"
            "----T--X--X***>-------X---===-----W--W-----"
            "-X->X-----=-------X---X>----WWW==----------"
            "----X--X--X>-->---X>--X----=========-------"
            "-X-----X-----------X>-------WWW==----------"
            "-X--T--X--X***--T--X------WW>---------W----"
            "XXX----X>-X***-T-T-X-------WWWWWWWWWWWWW---"
            "-X-----X--X***-----X---*---WW****WWW*WW*WWW"
        |]
    |]

#endif

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NumberOfRowsForTankMatrix = 9
let SandGap = "-----"

[<Struct>]
type EnemyTankTileLocation =
    {
        etx: int
        ety: int
    }

type TankBattleMapMatrix =
    {
        TilesHorizontally   : int
        // TODO:  At the moment, just use the constant:   TilesVertically     : int
        TilesArray          : ImageID []
        EnemyTanks          : EnemyTankTileLocation list
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private CharacterToTile ch =
    match ch with
        | '-' -> Some(ImageTileSand)
        | '=' -> Some(ImageTileBridge)
        | 'X' -> Some(ImageTileBarricade)
        | 'W' -> Some(ImageTileWater)
        | '*' -> Some(ImageTileMine)
        | 'T' -> Some(ImageTileTower)
        | '>' -> Some(ImageTileSand)       // Because enemy tanks are not tile sprites.
        | _   -> None


let private ContainsLegalTileCharacters str =
    str |> String.forall (CharacterToTile >> Option.isSome)


let private MatrixRowsAreAllTheSameLength (strings:string[]) =
    strings.Length >= NumberOfRowsForTankMatrix
        && [0..(NumberOfRowsForTankMatrix - 1)] 
            |> List.forall (fun i -> strings.[i].Length = strings.[0].Length)


let private MatrixHasLegalCharacters (strings:string[]) =
    strings.Length >= NumberOfRowsForTankMatrix
        && strings.[0..(NumberOfRowsForTankMatrix - 1)]
            |> Array.forall ContainsLegalTileCharacters


let private TranslateToTileArray (strings:string[]) =
    seq {
            for i in 0..(NumberOfRowsForTankMatrix - 1) do
                yield strings.[i] 
                    |> Seq.map (CharacterToTile >> (Option.defaultValue ImageTileSand)) 
                    |> Seq.toArray
        }
        |> Array.concat



let private TranslateToEnemyTankLocationList (strings:string[]) =

    if strings.Length > 0 then

        let numberOfColumnsForTankMatrix = strings.[0].Length

        seq {
                for y in 0..(NumberOfRowsForTankMatrix - 1) do
                    let thisRow = strings.[y]
                    for x in 0..(numberOfColumnsForTankMatrix - 1) do
                        let thisTile = thisRow.[x]
                        if thisTile = '>' then
                            yield { etx=x ; ety=y }
            }
            |> Seq.toList

    else
        []





let private LoadTileFileFromStrings (strings:string[]) =

    if strings.Length < NumberOfRowsForTankMatrix then
        Error "Map matrix file has insufficient rows."

    else if not (strings |> MatrixRowsAreAllTheSameLength) then
        Error "The map matrix rows must all be the same length."

    else if not (strings |> MatrixHasLegalCharacters) then
        Error "The map matrix contains illegal characters."

    else

        let withSandAddedEitherSide (strings:string[]) =
            strings.[0..NumberOfRowsForTankMatrix-1] |> Array.map (fun s -> SandGap + s + SandGap)

        let strings = strings |> withSandAddedEitherSide

        Ok 
            {
                TilesHorizontally   = strings.[0].Length
                // TilesVertically     = NumberOfRowsForTankMatrix  // TODO: future possibility.
                TilesArray          = strings |> TranslateToTileArray
                EnemyTanks          = strings |> TranslateToEnemyTankLocationList
            }


let private LoadTankBattleTileFileFromIndex i =

    TankMapDefinitions.[i]
        |> LoadTileFileFromStrings 
        |> Result.mapError (fun msg -> (sprintf "TankMapDefinitions.[%d]: Error: %s" i msg))



let LoadTankBattleSequences () =

    let whetherItLoadedOkOrNot result =
        match result with
            | Ok tankMap -> FirstList tankMap
            | Error msg  -> SecondList msg

    #if SHORT_PLAYTHROUGH
    [0]
    #else
    [0..3]
    #endif
        |> List.map LoadTankBattleTileFileFromIndex
        |> ListSplicedBy whetherItLoadedOkOrNot
        |> (fun struct (tankMaps,failMessages) -> 
            match failMessages with
                | []   -> Ok tankMaps
                | msgs -> Error (msgs |> String.concat System.Environment.NewLine))

