module MazeState

open ResourceIDs
open MazeFilter  // TODO: only needed for the MazeByte type for the array declarations.

/// Indexes the tiles graphic.
[<Struct>]
type MazeTile = MazeTile of byte   // TODO: Can we specify representation on the enum type?

type MazeState =
    {
        MazeTilesCountX  : int
        MazeTilesCountY  : int
        MazeTiles        : MazeTile []  // content mutated by pacman eating things // TODO: FP
        MazeGhostRails   : MazeByte []  // not mutated
        MazePlayersRails : MazeByte []  // not mutated
    }

/// Asks whether the given tile is something pacman eats.
let private IsSomethingPacManNeedsToEat (MazeTile(tile)) =
    (tile = ((byte)TileIndex.Pill1)) || (tile = ((byte)TileIndex.Dot))   // NB: Pill2 is never encoded in the maze itself.

/// Maze property - are all dots and pills eaten?
let IsAllEaten maze =
    not  (maze.MazeTiles |> Array.exists (fun tile -> tile |> IsSomethingPacManNeedsToEat))

