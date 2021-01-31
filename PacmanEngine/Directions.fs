module Directions

open Geometry
open MazeFilter

// TODO: Amalgamate with the library's Directions module.
type FacingDirection = FacingLeft | FacingUp | FacingRight | FacingDown

let FacingDirectionToBitMaskByte facingDirection = // TODO: rename
    match facingDirection with
        | FacingLeft  -> MazeByteLeft
        | FacingRight -> MazeByteRight
        | FacingUp    -> MazeByteUp
        | FacingDown  -> MazeByteDown

let FacingDirectionToInt facingDirection =
    match facingDirection with
        | FacingLeft  -> 0
        | FacingUp    -> 1
        | FacingRight -> 2
        | FacingDown  -> 3

let inline DirectionToMovementDelta zero i facingDirection =
    match facingDirection with
        | FacingLeft  -> { modx = -i    ; mody =  zero }
        | FacingRight -> { modx =  i    ; mody =  zero }
        | FacingUp    -> { modx =  zero ; mody = -i    }
        | FacingDown  -> { modx =  zero ; mody =  i    }
        
let DirectionToMovementDeltaI32 =
    DirectionToMovementDelta 0<epx> 1<epx> 
        