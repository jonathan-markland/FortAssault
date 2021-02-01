module Directions

/// Four-way directions, clockwise definition.
type FourWayDirection  = Left4=0 | Up4=1 | Right4=2 | Down4=3

/// Eight-way directions, clockwise definition.
type EightWayDirection = Left8=0 | UpLeft8=1 | Up8=2 | UpRight8=3 | Right8=4 | DownRight8=5 | Down8=6 | DownLeft8=7

/// Direction deltas for 8-way, with X axis running to the right, Y axis running downwards.
let DeltasForEightWayDirection eightWayDirection =
    match eightWayDirection with
        | EightWayDirection.Left8      -> -1, 0
        | EightWayDirection.UpLeft8    -> -1,-1
        | EightWayDirection.Up8        ->  0,-1
        | EightWayDirection.UpRight8   ->  1,-1
        | EightWayDirection.Right8     ->  1, 0
        | EightWayDirection.DownRight8 ->  1, 1
        | EightWayDirection.Down8      ->  0, 1
        | EightWayDirection.DownLeft8  -> -1, 1
        | _ -> failwith "Cannot map DeltasForEightWayDirection"











