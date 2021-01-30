module GamePlayDataModels

open Geometry
open ScoreHiScore
open FlickBook

// Between stories data model - lives, score, hiscore

// The Man state dictates progress of the gameplay
// If the man is electrocuting, there would be a countdown timer, and
// that should NOT be handled by an "untilTime" in these records, do with GameStateManagement?

/// Level space is in pixels, and covers the area of the entire level.
[<Measure>]
type LevelSpace

/// An integer Cartesian point in "level space", which is pixel-based.
/// The level is a 4 x 4 arrangement of screens, each screen is 25 x 25 bricks
/// where each brick is 12 x 8 pixels.
type LevelPoint = Point<int<LevelSpace>>

/// A point on the screen (based on float32<epx>)
type ScreenPoint = Point<float32<epx>>

/// Position of a bullet.
type Bullet =
    {
        BulletCentrePosition : ScreenPoint
    }

/// The kinds of item that the player must collect.
type InventoryObjectType = InvKey | InvRing | InvGold

/// An item that the player must collect on the current level.
type Collectible =
    {
        CollectibleType           : InventoryObjectType
        CollectibleCentrePosition : ScreenPoint
    }

/// Basic states the player can be in during the gameplay screen.
type ManState = ManAlive | ManElectrocuting | ManDead

/// The player's state.
type ManModel =
    {
        /// The basic state of the player.
        ManState    : ManState

        /// The centre position of the player on the screen.
        ManCentrePosition : ScreenPoint
    }


/// Types of droid.  This affects visuals and behaviours.
type DroidType = HomingDroid | WanderingDroid | AssassinDroid


/// Model for droid adversaries on the current screen.
type DroidModel =
    {
        /// The type of droid
        DroidType     : DroidType

        /// The centre position of the droid on the screen.
        DroidCentrePosition : ScreenPoint
    }


/// Is the ghost visible or not?
type GhostModel =
    | NoGhost
    | GhostActive of ScreenPoint


/// The number of lives the player has remaining.
type ManLives = ManLives of int

/// During gameplay - the current level number.
type LevelNumber = LevelNumber of int

/// During gameplay - the screen number that the gameplay is showing.
type ScreenNumber = ScreenNumber of int



/// The data model for the inner screen.
/// Garbage:  Changes less often!
type InnerScreenModel =
    {
        /// Current level number.  Displayed at top of screen.
        LevelNumber       : LevelNumber

        /// Current screen number within the level.  For display at top of screen.
        ScreenNumber      : ScreenNumber

        /// The level-pixel (x,y) of the top left of the top left brick, of the current screen.
        ScreenOriginPixel : LevelPoint

        /// Index into the 2D level tile matrix of the top left brick, of the current screen.
        ScreenOriginBlock : int * int

        /// The player's score, and hi-score.
        ScreenScore       : ScoreAndHiScore

        /// The player's inventory.
        ManInventory      : InventoryObjectType list

        /// The number of lives that the player has left.
        ManLives          : ManLives

        /// The locations within the current level of the collectible items.
        Collectibles      : Collectible list
    }



/// The data model for the gameplay screen.
type ScreenModel =
    {
        /// The state of the player
        ScreenMan         : ManModel

        /// The state of the droids on the current screen.
        ScreenDroids      : DroidModel list

        /// The state of the ghost on the current screen.
        ScreenGhost       : GhostModel

        /// The state of the bullets on the current screen.
        Bullets           : Bullet list

        /// Purely decorative flickbook animations on the current screen.
        DecorativeFlickbooks : FlickBookInstance list
    }


