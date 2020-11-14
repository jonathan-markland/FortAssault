module Directions

open MazeFilter

type FacingDirection = FacingLeft | FacingUp | FacingRight | FacingDown

let FacingDirectionToBitMaskByte facingDirection =  // TODO: Type model for the bitmask "byte"
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

