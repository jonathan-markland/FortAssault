module LevelTextToMatrix

open GamePlayDataModels  // TODO: Don't need all the models!
open GamePlayScreenConstants


let LevelTextToMatrix levelTextArray =

    let toTileIndexType ch =
        match ch with
            | '.' -> TileIndex.TileFloor1
            | ':' -> TileIndex.TileFloor2
            | '#' -> TileIndex.TileWall1 
            | '@' -> TileIndex.TileWall2 
            | '*' -> TileIndex.TileEdge1 
            | '+' -> TileIndex.TileEdge1  // TODO: Reinstate if the glowing supports all tile types. TileEdge2 
            | _   -> failwith "Unrecognised character in level definition text"

    
    levelTextArray 

        |> Array.map (fun (rowString:string) ->
            if rowString.Length = (NumBricksPerSide * NumRoomsPerSide) then
                rowString |> Seq.map toTileIndexType |> Seq.toArray
            else
                failwith "Level definition has incorrect horizontal dimension.")

        |> LevelTileMatrix

