module GamePlayScreenConstants

open Geometry
open Time

// This game uses a the following engine-coordinate (epx) space:

let ScreenWidthInt  = 320<epx>  // Must be the same as below!
let ScreenHeightInt = 256<epx>  // Must be the same as below!

let ScreenWidth  = 320.0F<epx>
let ScreenHeight = 256.0F<epx>

let inline PercentOfScreenWidth  x = (ScreenWidthInt * x) / 100
let inline PercentOfScreenHeight x = (ScreenHeightInt * x) / 100

let BrickTileWidth  = 12<epx>
let BrickTileHeight =  8<epx>

let NumBricksPerSide = 25
let NumRoomsPerSide  = 4

let RoomWidthPixelsInt  = NumBricksPerSide * BrickTileWidth
let RoomHeightPixelsInt = NumBricksPerSide * BrickTileHeight
let RoomWidthPixels     = RoomWidthPixelsInt  |> IntToFloatEpx
let RoomHeightPixels    = RoomHeightPixelsInt |> IntToFloatEpx

let PlayAreaOffsetX = (ScreenWidthInt  - (NumBricksPerSide * BrickTileWidth)) / 2
let PlayAreaOffsetY = (ScreenHeightInt - (NumBricksPerSide * BrickTileHeight)) / 2

let GhostTriggerDistance         = 12.0F<epx>
let DroidTriggerDistance         = 12.0F<epx>
let BulletTriggerDistance        =  8.0F<epx>
let InteractibleTriggerDistance  = 10.0F<epx>
let ManVsWallTriggerDistance     =  8.0F<epx>
let DroidVsWallTriggerDistance   =  6.0F<epx>
let DroidVsDroidTriggerDistance  = 12.0F<epx>
let DroidVsManTriggerDistance    = DroidTriggerDistance + 2.0F<epx>  // Stop droid moving inside man's collision distance.
let BulletVsWallsTriggerDistance =  1.0F<epx>
let ManExclusionRectangleExtension = 80<epx>

/// Used for initial positioning of adversaries in the room.
let LargestAdversaryDimension    = 20<epx>

/// Used for initial positioning of interactibles throughout the level.
let LargestInteractibleDimension = 25<epx>

let ManFiringStartDistance      = 10.0F    // Used as multiplier hence no units.
let DroidFiringStartDistance    = 8.0F     // Used as multiplier hence no units.

let HomingDroidSpeed    = 0.6F<epx>
let WanderingDroidSpeed = 0.9F
let AssassinDroidSpeed  = 0.45F<epx>
let BulletSpeed         = 4.0F
let ManSpeed            = 1.0F
let GhostSpeed          = 2.0F<epx>

let WanderingDroidDecisionInterval =  3.0F<seconds>
let GhostGraceDuration             = 20.0F<seconds>
let GhostStunDuration              =  4.0F<seconds>
let LevelExitPauseDuration         =  2.0F<seconds>
let LevelIntroCardDuration         =  5.0F<seconds>
let LifeLossPauseDuration          =  2.0F<seconds>
let InvincibilityEndDuration       =  5.0F<seconds>
let InvincibilityDuration          = InvincibilityEndDuration + 15.0F<seconds>

/// Distance in from the sides at which to position man when flipping between rooms.
let ManRoomFlipMargin = ManSpeed * 4.0F<epx>

/// For display of score text (etc) at top of screen.
let TopPanelTopY =  10<epx>

/// For display of lives, inventory (etc) at bottom of screen.
let BottomPanelTopY = 238<epx>

/// Indent for text from the side of the screen, used for left and right alignment.
let TextIndent = 10<epx>

/// Spacing between inventory items in the inventory display.
let InventoryItemSpacing = 20<epx>

/// Spacing between lives in the inventory display.
let LifeItemSpacing = 3<epx>

let ManWalkingStepsPerSecond = 5.0F
let ManElectrocutionSwitchesPerSecond = 8.0F
let WallElectrocutionSwitchesPerSecond = 20.0F
let DroidAnimationPerSecond = 6.0F
let InvincibilityFlashesPerSecond = 12.0F
let InvincibilityWaningFlashesPerSecond = 4.0F

let ScoreBonusForShootingAllDroids = 250u
