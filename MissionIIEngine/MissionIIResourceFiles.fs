module MissionIIResourceFiles

open ImagesAndFonts
open Sounds
open Geometry

let private image colourKey fileName width height =
    {
        ImageTransparency = colourKey
        ImageFileName  = fileName
        ImageWidth     = width  |> IntToIntEpx
        ImageHeight    = height |> IntToIntEpx
    }

let PacmanFontResourceImages =
    [
    need the dimensions adding
        image MagentaColourKeyImage "Font.png"     296 8
    ]

let PacmanResourceImages =
    [
    need the dimensions adding
        image OpaqueImage           "Background.png"
        image OpaqueImage           "Background2.png"
        image MagentaColourKeyImage "Bullet.png"
        image MagentaColourKeyImage "Dead.png"
        image MagentaColourKeyImage "Electrocution_1.png"
        image MagentaColourKeyImage "Electrocution_2.png"
        image MagentaColourKeyImage "Explosion_1.png"
        image MagentaColourKeyImage "Explosion_2.png"
        image MagentaColourKeyImage "Explosion_3.png"
        image MagentaColourKeyImage "FacingDown.png"
        image MagentaColourKeyImage "FacingLeft.png"
        image MagentaColourKeyImage "FacingLeftDown.png"
        image MagentaColourKeyImage "FacingLeftUp.png"
        image MagentaColourKeyImage "FacingRight.png"
        image MagentaColourKeyImage "FacingRightDown.png"
        image MagentaColourKeyImage "FacingRightUp.png"
        image MagentaColourKeyImage "FacingUp.png"
        image MagentaColourKeyImage "FloorTile_1.png"
        image MagentaColourKeyImage "FloorTile_2.png"
        image MagentaColourKeyImage "FloorTile_3.png"
        image MagentaColourKeyImage "Ghost.png"
        image MagentaColourKeyImage "GhostStunned.png"
        image MagentaColourKeyImage "Gold.png"
        image MagentaColourKeyImage "InvincibilityAmulet.png"
        image MagentaColourKeyImage "Key.png"
        image OpaqueImage           "LevelExit.png"
        image MagentaColourKeyImage "Life.png"
        image MagentaColourKeyImage "Monster1_1.png"
        image MagentaColourKeyImage "Monster1_2.png"
        image MagentaColourKeyImage "Monster2_1.png"
        image MagentaColourKeyImage "Monster2_2.png"
        image MagentaColourKeyImage "Monster3.png"
        image MagentaColourKeyImage "Monster4_1.png"
        image MagentaColourKeyImage "Monster4_2.png"
        image MagentaColourKeyImage "Monster5_1.png"
        image MagentaColourKeyImage "Monster5_2.png"
        image OpaqueImage           "Paused.png"
        image MagentaColourKeyImage "Potion_1.png"
        image MagentaColourKeyImage "Potion_2.png"
        image MagentaColourKeyImage "Ring.png"
        image MagentaColourKeyImage "WalkingDown_1.png"
        image MagentaColourKeyImage "WalkingDown_2.png"
        image MagentaColourKeyImage "WalkingLeftDown_1.png"
        image MagentaColourKeyImage "WalkingLeftDown_2.png"
        image MagentaColourKeyImage "WalkingLeftUp_1.png"
        image MagentaColourKeyImage "WalkingLeftUp_2.png"
        image MagentaColourKeyImage "WalkingLeft_1.png"
        image MagentaColourKeyImage "WalkingLeft_2.png"
        image MagentaColourKeyImage "WalkingRightDown_1.png"
        image MagentaColourKeyImage "WalkingRightDown_2.png"
        image MagentaColourKeyImage "WalkingRightUp_1.png"
        image MagentaColourKeyImage "WalkingRightUp_2.png"
        image MagentaColourKeyImage "WalkingRight_1.png"
        image MagentaColourKeyImage "WalkingRight_2.png"
        image MagentaColourKeyImage "WalkingUp_1.png"
        image MagentaColourKeyImage "WalkingUp_2.png"
        image OpaqueImage           "WallBrick_1.png"
        image OpaqueImage           "WallBrick_2.png"
        image OpaqueImage           "WallBrick_3.png"
        image OpaqueImage           "WallElectric.png"
        image OpaqueImage           "WallOutline_1.png"
        image OpaqueImage           "WallOutline_2.png"
        image OpaqueImage           "WallOutline_3.png"
    ]

let PacmanResourceSounds : SoundMetadata list =
    [
        { SoundFileName = "BonusSound.ogg" }
        { SoundFileName = "DroidFiringSound.ogg" }
        { SoundFileName = "DuoBonusSound.ogg" }
        { SoundFileName = "ElectrocutionSound.ogg" }
        { SoundFileName = "EnteringLevelSound.ogg" }
        { SoundFileName = "ExplosionSound.ogg" }
        { SoundFileName = "ExtraLifeSound.ogg" }
        { SoundFileName = "Footstep1Sound.ogg" }
        { SoundFileName = "Footstep2Sound.ogg" }
        { SoundFileName = "GameOverSound_1.ogg" }
        { SoundFileName = "GameOverSound_2.ogg" }
        { SoundFileName = "GameOverSound_3.ogg" }
        { SoundFileName = "GameOverSound_4.ogg" }
        { SoundFileName = "GhostAppearingSound.ogg" }
        { SoundFileName = "IntroSound.ogg" }
        { SoundFileName = "InvincibilityAmuletSound.ogg" }
        { SoundFileName = "LevelExitActivatedSound.ogg" }
        { SoundFileName = "ManFiringSound.ogg" }
        { SoundFileName = "ManGruntSound_1.ogg" }
        { SoundFileName = "ManGruntSound_2.ogg" }
        { SoundFileName = "PauseModeSound.ogg" }
        { SoundFileName = "PickUpObjectSound.ogg" }
        { SoundFileName = "StunGhostSound.ogg" }

    ]
