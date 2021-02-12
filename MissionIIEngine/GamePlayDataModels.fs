module GamePlayDataModels

open GameStateManagement
open Time
open Geometry
open ScoreHiScore
open FlickBook
open Directions
open ImagesAndFonts
open Tiles


/// The number of lives the player has remaining.
type ManLives = ManLives of uint32

/// During gameplay - the current level number.
type LevelNumber = LevelNumber of int

/// During gameplay - the screen number that the gameplay is showing.
type RoomNumber = RoomNumber of int

/// Wall and floor tile matrix type.
type TileIndex =
    | TileFloor1 =  0uy  // Where the man can walk
    | TileFloor2 =  1uy  // Where the man can walk
    | TileWall1  =  2uy  // The inner wall (brick style 1)
    | TileWall2  =  3uy  // The inner wall (brick style 2)  TODO: Reduce to just 1 when we add patterning-by-resampling solution
    | TileEdge1  =  4uy  // The electrocution edge (brick style 1)
    | TileEdge2  =  5uy  // The electrocution edge (brick style 2)  TODO: Reduce to just 1 when we add patterning-by-resampling solution

/// The level matrix when converted from the Levels.fs file's string format.
type LevelTileMatrix = LevelTileMatrix of TileIndex [][]

// TODO: The Man state dictates progress of the gameplay
// If the man is electrocuting, there would be a countdown timer, and
// that should NOT be handled by an "untilTime" in these records, do with GameStateManagement?

/// A point relative to the top left pixel of the room.
/// In essence, the game play coordinates.
type ViewPoint = ViewPoint of Point<float32<epx>>

/// Position and direction of bullet
type Bullet =
    {
        /// Position of bullet
        BulletCentrePosition : ViewPoint

        /// Direction of travel of bullet
        BulletDirection      : EightWayDirection
    }

/// The kinds of item that the player can hold in the inventory.
type InventoryObjectType = InvKey | InvRing | InvGold

/// Items scattered about the level that the player can interact with in some way.
type InteractibleObjectType = ObKey=0 | ObRing=1 | ObGold=2 | ObAmulet=3 | ObHealthBonus=4 | ObLevelExit=5

/// An item that the player must collect on the current level.
type Interactible =
    {
        InteractibleRoom           : RoomNumber
        InteractibleType           : InteractibleObjectType
        InteractibleCentrePosition : ViewPoint
    }

/// Basic states the player can be in during the gameplay screen.
type ManState = 
    | ManStandingFacing of facing:EightWayDirection
    | ManWalking        of facing:EightWayDirection
    | ManElectrocuted
    | ManDead

/// The player's state.
type ManModel =
    {
        /// The basic state of the player.
        ManState    : ManState

        /// The centre position of the player on the screen.
        ManCentrePosition : ViewPoint
    }


/// Types of droid.  This affects visuals and behaviours.
type DroidType = 
    | HomingDroid
    | WanderingDroid of movement:EightWayDirection * nextDirectionChangeTime:float32<seconds>
    | AssassinDroid


/// Model for droid adversaries on the current screen.
type DroidModel =
    {
        /// The type and traits-related state of droid
        DroidType : DroidType

        /// The centre position of the droid on the screen.
        DroidCentrePosition : ViewPoint
    }


/// Is the ghost visible or not?
type GhostModel =
    | NoGhostUntil of gameTime:float32<seconds>
    | GhostActive of ViewPoint
    | GhostStunned of ViewPoint * untilGameTime:float32<seconds>


type ImageLookupsTables =
    {
        /// Brick images for use by the drawing function on this level.
        BrickStyles       : Image[]

        /// Image lookup for use when the man is standing, facing a direction.
        ManFacingStyles   : Image[]

        /// Image lookup for use when the man is walking, animation step 1.
        ManWalkingStyles1 : Image[]

        /// Image lookup for use when the man is walking, animation step 2.
        ManWalkingStyles2 : Image[]

        /// Image lookup for droids, animation step 1.
        DroidStyles1      : Image[]

        /// Image lookup for droids, animation step 2.
        DroidStyles2      : Image[]

        /// Image lookup for interactive objects.
        InteractibleObjectStyles : Image[]
    }


type LevelModel =
    {
        /// Current level number.  Displayed at top of screen.
        LevelNumber       : LevelNumber

        /// The tiles matrix for the current level.
        LevelTileMatrix   : LevelTileMatrix

        /// TileMatrixTraits for rooms (cached).
        TileMatrixTraits  : TileMatrixTraits
    }



type RoomReference =  // TODO: rename "RoomModel"
    {
        /// Current screen number within the level.  For display at top of screen.
        RoomNumber        : RoomNumber

        /// Indicates the current room by index into the matrix ie: (0..3,0..3)
        RoomOrigin        : int * int

        /// The current level data
        LevelModel        : LevelModel
    }



/// The data model for the inner screen.
/// Garbage:  Changes less often!
type InnerScreenModel =
    {
        /// A reference to the current room within the level matrix.
        RoomReference     : RoomReference

        /// The player's score, and hi-score.
        ScreenScore       : ScoreAndHiScore

        /// The player's inventory.
        ManInventory      : InventoryObjectType list

        /// The number of lives that the player has left.
        ManLives          : ManLives

        /// The locations within the current level items the player can interact with.
        Interactible      : Interactible list

        /// Lookup tables for images when drawing.
        ImageLookupsTables : ImageLookupsTables

        /// Constructor for GameOver
        WhereToOnGameOver : ScoreAndHiScore -> float32<seconds> -> ErasedGameState
    }


/// The data model for the gameplay screen.
type ScreenModel =
    {
        /// The things that don't change often.
        InnerScreenModel  : InnerScreenModel

        /// The state of the player
        ScreenMan         : ManModel

        /// The state of the droids on the current screen.
        ScreenDroids      : DroidModel list

        /// The state of the ghost on the current screen.
        ScreenGhost       : GhostModel

        /// The state of the bullets (fired by the player) on the current screen.
        ManBullets        : Bullet list
        
        /// The state of the bullets (fired by the droid) on the current screen.
        DroidBullets      : Bullet list

        /// Purely decorative flickbook animations on the current screen.
        DecorativeFlickbooks : FlickBookInstance list
    }


