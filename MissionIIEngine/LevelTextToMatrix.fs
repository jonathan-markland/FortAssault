module LevelTextToMatrix

open GamePlayDataModels  // TODO: Don't need all the models!
open GamePlayScreenConstants



let private ClusterSize = 3



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




let private BlankFloorAt (LevelTileMatrix levelTileMatrix) ix iy =

    let scoreForTile tile =
        match tile with
            | TileIndex.TileFloor1 -> 1
            | TileIndex.TileFloor2 -> 1
            | _ -> 0

    seq {
        for y in iy..(iy + ClusterSize-1) do
            for x in ix..(ix + ClusterSize-1) do
                yield levelTileMatrix.[y].[x]
    }
    |> Seq.sumBy scoreForTile
    |> (=) (ClusterSize * ClusterSize)


type private CoordinateScheme = RoomRelativeCoordinates | LevelRelativeCoordinates

let private PlacesWhereObjectsCanBePlaced coordinateScheme levelTileMatrix (roomX,roomY) =

    let centreOffset = ClusterSize / 2
    let count = NumBricksPerSide / ClusterSize

    seq {

        let roomOffsetX = roomX * NumBricksPerSide
        let roomOffsetY = roomY * NumBricksPerSide

        for clusterY in 0..(count-1) do
            for clusterX in 0..(count-1) do

                let roomTileX = clusterX * ClusterSize
                let roomTileY = clusterY * ClusterSize
            
                if BlankFloorAt levelTileMatrix (roomOffsetX + roomTileX) (roomOffsetY + roomTileY) then

                    let x' = roomTileX + centreOffset
                    let y' = roomTileY + centreOffset

                    yield 
                        match coordinateScheme with
                            | RoomRelativeCoordinates  -> (x', y')
                            | LevelRelativeCoordinates -> (x'+roomOffsetX , y'+roomOffsetY)
    }



let PlacesWhereObjectsCanBePlacedInRoom levelTileMatrix roomOrigin =
    PlacesWhereObjectsCanBePlaced RoomRelativeCoordinates levelTileMatrix roomOrigin



let PlacesWhereObjectsCanBeLocatedInLevel levelTileMatrix =
    seq {
        for roomY in 0..(NumRoomsPerSide-1) do
            for roomX in 0..(NumRoomsPerSide-1) do
                yield! PlacesWhereObjectsCanBePlaced LevelRelativeCoordinates levelTileMatrix (roomX, roomY)
    }


