module TitleScreenShared

open Geometry
open PacmanShared
open ResourceIDs
open DrawingShapes

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Models
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// So a title screen's model constructor can memoize the position of a pacman image.
type TitleScreenPacmanState =
    {
        pos       : Point<int<epx>>
        direction : FacingDirection
    }

/// So a title screen's model constructor can memoize the position of a ghost image.
type TitleScreenGhostState =
    {
        pos    : Point<int<epx>>
        number : GhostNumber
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Drawing functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Intended for title screens, game over screen, not gameplay.
let DrawPacMan render tilesImage gameTime pacmanState =
    let pillMode = false
    let { pos=pos ; direction=facingDirection } = pacmanState
    DrawPacManAlive render tilesImage pos facingDirection pillMode gameTime

/// Intended for title screens, game over screen, not gameplay.
let DrawGhost render tilesImage gameTime ghostState =
    let { pos=pos ; number=ghostNumber } = ghostState
    DrawGhost render tilesImage pos ghostNumber GhostNormal gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Constructors
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let TitleScreenPac facing percentX percentY =

    let cx = percentX |> PercentOfScreenWidth
    let cy = percentY |> PercentOfScreenHeight
    let h  = TileSide / 2

    { 
        pos = { ptx = cx - h ; pty = cy - h }
        direction = facing
    }

let TitleScreenGhost ghostNumber percentX percentY =

    let cx = percentX |> PercentOfScreenWidth
    let cy = percentY |> PercentOfScreenHeight
    let h  = TileSide / 2

    { 
        pos = { ptx = cx - h ; pty = cy - h }
        number = ghostNumber
    }

