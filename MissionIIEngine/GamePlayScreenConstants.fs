module GamePlayScreenConstants

open Geometry

// This game uses a the following engine-coordinate (epx) space:

let ScreenWidthInt  = 320<epx>  // Must be the same as below!
let ScreenHeightInt = 256<epx>  // Must be the same as below!

let ScreenWidth  = 320.0F<epx>
let ScreenHeight = 256.0F<epx>

let inline PercentOfScreenWidth  x = (ScreenWidthInt * x) / 100
let inline PercentOfScreenHeight x = (ScreenHeightInt * x) / 100

let BrickTileWidth  = 12<epx>
let BrickTileHeight = 12<epx>

let NumBricksPerSide = 25
let NumRoomsPerSide  = 4

let PlayAreaOffsetX = (ScreenWidthInt  - (NumBricksPerSide * BrickTileWidth)) / 2
let PlayAreaOffsetY = (ScreenHeightInt - (NumBricksPerSide * BrickTileHeight)) / 2

/// For display of score text (etc) at top of screen.
let TopPanelTopY =  10<epx>

/// For display of lives, inventory (etc) at bottom of screen.
let BottomPanelTopY = 238<epx>

/// Indent for text from the side of the screen, used for left and right alignment.
let TextIndent = 10<epx>

/// Spacing between inventory items in the inventory display.
let InventoryItemSpacing = 20<epx>

let ManWalkingStepsPerSecond = 3.0F
let ManElectrocutionSwitchesPerSecond = 8.0F
let DroidAnimationPerSecond = 2.0F
