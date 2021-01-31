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

let private font fileName width charWidth charHeight =
    {
        FontImageMetadata = image MagentaColourKeyImage fileName width charHeight
        FontCharWidth     = charWidth
    }

let MissionIIFontResourceImages =
    [
        font "Font.png"     216 6 8
    ]

let MissionIIResourceImages =
    [
        image OpaqueImage           "Background.png"         320 256
        image OpaqueImage           "Background2.png"        320 256
        image MagentaColourKeyImage "Bullet.png"               4   4
        image MagentaColourKeyImage "Dead.png"                18  14
        image MagentaColourKeyImage "Electrocution_1.png"     14  18
        image MagentaColourKeyImage "Electrocution_2.png"     14  18
        image MagentaColourKeyImage "Explosion_1.png"         14  10
        image MagentaColourKeyImage "Explosion_2.png"         14  10
        image MagentaColourKeyImage "Explosion_3.png"         14  10
        image MagentaColourKeyImage "FacingDown.png"          14  18
        image MagentaColourKeyImage "FacingLeft.png"          14  18
        image MagentaColourKeyImage "FacingLeftDown.png"      14  18
        image MagentaColourKeyImage "FacingLeftUp.png"        14  18
        image MagentaColourKeyImage "FacingRight.png"         14  18
        image MagentaColourKeyImage "FacingRightDown.png"     14  18
        image MagentaColourKeyImage "FacingRightUp.png"       14  18
        image MagentaColourKeyImage "FacingUp.png"            14  18
        image MagentaColourKeyImage "FloorTile_1.png"         12   8
        image MagentaColourKeyImage "FloorTile_2.png"         12   8
        image MagentaColourKeyImage "FloorTile_3.png"         12   8
        image MagentaColourKeyImage "Ghost.png"               15  14
        image MagentaColourKeyImage "GhostStunned.png"        15  14
        image MagentaColourKeyImage "Gold.png"                 9  14
        image MagentaColourKeyImage "InvincibilityAmulet.png" 14  14
        image MagentaColourKeyImage "Key.png"                  8  12
        image OpaqueImage           "LevelExit.png"           12  16
        image MagentaColourKeyImage "Life.png"                 6   8
        image MagentaColourKeyImage "Monster1_1.png"          14  10
        image MagentaColourKeyImage "Monster1_2.png"          14  10
        image MagentaColourKeyImage "Monster2_1.png"          14  10
        image MagentaColourKeyImage "Monster2_2.png"          14  10
        image MagentaColourKeyImage "Monster3.png"            14  10
        image MagentaColourKeyImage "Monster4_1.png"          14  10
        image MagentaColourKeyImage "Monster4_2.png"          14  10
        image MagentaColourKeyImage "Monster5_1.png"          10  10
        image MagentaColourKeyImage "Monster5_2.png"          10  10
        image OpaqueImage           "Paused.png"              80  24
        image MagentaColourKeyImage "Potion_1.png"            12  12
        image MagentaColourKeyImage "Potion_2.png"            12  12
        image MagentaColourKeyImage "Ring.png"                14  10
        image MagentaColourKeyImage "WalkingDown_1.png"       14  18
        image MagentaColourKeyImage "WalkingDown_2.png"       14  18
        image MagentaColourKeyImage "WalkingLeftDown_1.png"   14  18
        image MagentaColourKeyImage "WalkingLeftDown_2.png"   14  18
        image MagentaColourKeyImage "WalkingLeftUp_1.png"     14  18
        image MagentaColourKeyImage "WalkingLeftUp_2.png"     14  18
        image MagentaColourKeyImage "WalkingLeft_1.png"       14  18
        image MagentaColourKeyImage "WalkingLeft_2.png"       14  18
        image MagentaColourKeyImage "WalkingRightDown_1.png"  14  18
        image MagentaColourKeyImage "WalkingRightDown_2.png"  14  18
        image MagentaColourKeyImage "WalkingRightUp_1.png"    14  18
        image MagentaColourKeyImage "WalkingRightUp_2.png"    14  18
        image MagentaColourKeyImage "WalkingRight_1.png"      14  18
        image MagentaColourKeyImage "WalkingRight_2.png"      14  18
        image MagentaColourKeyImage "WalkingUp_1.png"         14  18
        image MagentaColourKeyImage "WalkingUp_2.png"         14  18
        image OpaqueImage           "WallBrick_1.png"         12   8
        image OpaqueImage           "WallBrick_2.png"         12   8
        image OpaqueImage           "WallBrick_3.png"         12   8
        image OpaqueImage           "WallBrick_4.png"         12   8
        image OpaqueImage           "WallBrick_5.png"         12   8
        image OpaqueImage           "WallBrick_6.png"         12   8
        image OpaqueImage           "WallBrick_7.png"         12   8
        image OpaqueImage           "WallBrick_8.png"         12   8
        image OpaqueImage           "WallElectric.png"        12   8
        image OpaqueImage           "WallOutline_1.png"       12   8
        image OpaqueImage           "WallOutline_2.png"       12   8
        image OpaqueImage           "WallOutline_3.png"       12   8
    ]

let MissionIIResourceSounds : SoundMetadata list =
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
