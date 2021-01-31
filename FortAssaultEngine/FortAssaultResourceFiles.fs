module FortAssaultResourceFiles

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

    
let FortAssaultFontResourceImages =
    [
        font "MilitaryFontRed.png"     296 8 8
        font "MilitaryFontYellow.png"  296 8 8
        font "MilitaryFontBlue.png"    296 8 8
        font "MilitaryFontBlack.png"   296 8 8
        font "MilitaryFontSymbols.png" 296 8 8
    ]

let FortAssaultResourceImages =
    [
        image OpaqueImage      "CliffsTile.png"          89 200 
        image MagentaColourKeyImage "Title.png"              297  20
        
        image OpaqueImage      "Map.png"                320 160
        image MagentaColourKeyImage "AlliedFleetSymbol.png"   21  21
        image MagentaColourKeyImage "EnemyFleetSymbol.png"    20  20
        
        image OpaqueImage      "SecretPassage.png"      320 160
        image MagentaColourKeyImage "Mine.png"                 6   8
        image MagentaColourKeyImage "Ship0.png"               40  13
        image MagentaColourKeyImage "Ship1.png"               34  18
        image MagentaColourKeyImage "Ship2.png"               31  21
        image MagentaColourKeyImage "Ship3.png"               26  26
        image MagentaColourKeyImage "Ship4.png"               12  36
        image MagentaColourKeyImage "Torpedo0.png"             8   2
        image MagentaColourKeyImage "Torpedo22degrees.png"     8   5
        image MagentaColourKeyImage "ShipExplode0.png"        25  25
        image MagentaColourKeyImage "ShipExplode1.png"        50  37
        image MagentaColourKeyImage "ShipExplode2.png"        50  49
        image MagentaColourKeyImage "ShipExplode3.png"        50  49
        
        image MagentaColourKeyImage "Plane0.png"               6  2
        image MagentaColourKeyImage "Plane1.png"              18  6
        image MagentaColourKeyImage "Plane2.png"              31 10
        image MagentaColourKeyImage "Plane3.png"              49 19
        image MagentaColourKeyImage "Plane4.png"              72 26
        image MagentaColourKeyImage "Plane4BankingLeft.png"   62 21
        image MagentaColourKeyImage "Plane4BankingRight.png"  62 21
        image MagentaColourKeyImage "Plane5.png"              73 22
        image MagentaColourKeyImage "Plane5BankingLeft.png"   66 20
        image MagentaColourKeyImage "Plane5BankingRight.png"  66 20
        image MagentaColourKeyImage "Plane6.png"              71 27
        image MagentaColourKeyImage "Plane6BankingLeft.png"   69 26
        image MagentaColourKeyImage "Plane6BankingRight.png"  69 26
        image MagentaColourKeyImage "PlaneExplode0.png"       45 23
        image MagentaColourKeyImage "PlaneExplode1.png"       51 26
        image MagentaColourKeyImage "PlaneExplode2.png"       35 19
        image MagentaColourKeyImage "Splash0.png"             12  9
        image MagentaColourKeyImage "PlaneBomb0.png"           2  2
        image MagentaColourKeyImage "PlaneBomb1.png"           4  4
        image MagentaColourKeyImage "PlaneBomb2.png"           6  6
        image MagentaColourKeyImage "PlaneBomb3.png"          10 10
        image MagentaColourKeyImage "PlaneBomb4.png"          12 12
        
        image MagentaColourKeyImage "SeaBattleBackground0.png"  320 160
        image MagentaColourKeyImage "SeaBattleBackground1.png"  320 160
        image MagentaColourKeyImage "SeaBattleBackground2.png"  320 160
        image MagentaColourKeyImage "SeaBattleBackground3.png"  320 160
        image MagentaColourKeyImage "EnemyShip0.png"             12  15
        image MagentaColourKeyImage "EnemyShip1.png"             32  16
        image MagentaColourKeyImage "EnemyShip2.png"             67  16
        image MagentaColourKeyImage "EnemyShip3.png"             12  15
        image MagentaColourKeyImage "EnemyShip4.png"             34  16
        
        image OpaqueImage      "TankBattleMissile.png"         12   1
        image OpaqueImage      "TileBarricade.png"             16  16
        image OpaqueImage      "TileBridge.png"                16  16
        image OpaqueImage      "TileMine.png"                  16  16
        image OpaqueImage      "TileSand.png"                  16  16
        image OpaqueImage      "TileTower.png"                 16  16
        image OpaqueImage      "TileWater.png"                 16  16
        image MagentaColourKeyImage "EnemyTankGun1.png"             24  12
        image MagentaColourKeyImage "TankFacingLeft0.png"           24  12
        image MagentaColourKeyImage "TankFacingUpLeft0.png"         24  14
        image MagentaColourKeyImage "TankFacingDownLeft0.png"       24  14
        image MagentaColourKeyImage "TankFacingLeft1.png"           24  12
        image MagentaColourKeyImage "TankFacingUpLeft1.png"         24  14
        image MagentaColourKeyImage "TankFacingDownLeft1.png"       24  14
        
        image OpaqueImage      "FinalBossBackground.png"      320 160
        image MagentaColourKeyImage "FinalBoss0.png"               207  43
        image MagentaColourKeyImage "FinalBoss1.png"               152  42
        image MagentaColourKeyImage "FinalBoss2.png"                94  42
        image MagentaColourKeyImage "FinalBoss3.png"                94  42
        image MagentaColourKeyImage "FinalBoss4.png"                94  42
        image MagentaColourKeyImage "FinalBoss5.png"                94  42
        image MagentaColourKeyImage "FinalBossSurrender0.png"       16  35
        image MagentaColourKeyImage "FinalBossSurrender1.png"       16  35
        image MagentaColourKeyImage "FinalBossActiveTarget.png"      4   4
        image MagentaColourKeyImage "FinalBossTarget.png"            4   4
        
        image MagentaColourKeyImage "ShipGunNozzle.png"             16   8
        image OpaqueImage      "ShipGunStem.png"               12   1
        image MagentaColourKeyImage "ShipGunBullet.png"             16   8
        image OpaqueImage      "IntermissionBackground.png"   320 200
        
        image OpaqueImage      "VictoryScreen.png"            320 200
        image MagentaColourKeyImage "ColonelReceivingMedal.png"     70 130
        image MagentaColourKeyImage "ColonelSaluting.png"           72 130
        image MagentaColourKeyImage "ColonelStanding.png"           50 130
        image MagentaColourKeyImage "SoldierSaluting.png"           54 120
        image MagentaColourKeyImage "SoldierStanding.png"           54 120
        image MagentaColourKeyImage "Medal.png"                      7  16
        image MagentaColourKeyImage "PresidentPresentingMedal.png"  66 130
        image MagentaColourKeyImage "PresidentSaluting.png"         62 130
        image MagentaColourKeyImage "PresidentStanding.png"         39 130
    ]

let FortAssaultResourceSounds : SoundMetadata list =
    [
    ]
