module FortAssaultImageFiles

open ResourceFileMetadata
open Geometry

let private image colourKey fileName width height =
    {
        ImageColourKey = colourKey
        ImageFileName  = fileName
        ImageWidth     = width  |> IntToIntEpx
        ImageHeight    = height |> IntToIntEpx
    }

let FortAssaultFontResourceImages =
    [
        image MagentaColourKey "MilitaryFontRed.png"     296 8
        image MagentaColourKey "MilitaryFontYellow.png"  296 8
        image MagentaColourKey "MilitaryFontBlue.png"    296 8
        image MagentaColourKey "MilitaryFontBlack.png"   296 8
        image MagentaColourKey "MilitaryFontSymbols.png" 296 8
    ]

let FortAssaultResourceImages =
    [
        image NoColourKey      "CliffsTile.png"          89 200 
        image MagentaColourKey "Title.png"              297  20
        
        image NoColourKey      "Map.png"                320 160
        image MagentaColourKey "AlliedFleetSymbol.png"   21  21
        image MagentaColourKey "EnemyFleetSymbol.png"    20  20
        
        image NoColourKey      "SecretPassage.png"      320 160
        image MagentaColourKey "Mine.png"                 6   8
        image MagentaColourKey "Ship0.png"               40  13
        image MagentaColourKey "Ship1.png"               34  18
        image MagentaColourKey "Ship2.png"               31  21
        image MagentaColourKey "Ship3.png"               26  26
        image MagentaColourKey "Ship4.png"               12  36
        image MagentaColourKey "Torpedo0.png"             8   2
        image MagentaColourKey "Torpedo22degrees.png"     8   5
        image MagentaColourKey "ShipExplode0.png"        25  25
        image MagentaColourKey "ShipExplode1.png"        50  37
        image MagentaColourKey "ShipExplode2.png"        50  49
        image MagentaColourKey "ShipExplode3.png"        50  49
        
        image MagentaColourKey "Plane0.png"               6  2
        image MagentaColourKey "Plane1.png"              18  6
        image MagentaColourKey "Plane2.png"              31 10
        image MagentaColourKey "Plane3.png"              49 19
        image MagentaColourKey "Plane4.png"              72 26
        image MagentaColourKey "Plane4BankingLeft.png"   62 21
        image MagentaColourKey "Plane4BankingRight.png"  62 21
        image MagentaColourKey "Plane5.png"              73 22
        image MagentaColourKey "Plane5BankingLeft.png"   66 20
        image MagentaColourKey "Plane5BankingRight.png"  66 20
        image MagentaColourKey "Plane6.png"              71 27
        image MagentaColourKey "Plane6BankingLeft.png"   69 26
        image MagentaColourKey "Plane6BankingRight.png"  69 26
        image MagentaColourKey "PlaneExplode0.png"       45 23
        image MagentaColourKey "PlaneExplode1.png"       51 26
        image MagentaColourKey "PlaneExplode2.png"       35 19
        image MagentaColourKey "Splash0.png"             12  9
        image MagentaColourKey "PlaneBomb0.png"           2  2
        image MagentaColourKey "PlaneBomb1.png"           4  4
        image MagentaColourKey "PlaneBomb2.png"           6  6
        image MagentaColourKey "PlaneBomb3.png"          10 10
        image MagentaColourKey "PlaneBomb4.png"          12 12
        
        image MagentaColourKey "SeaBattleBackground0.png"  320 160
        image MagentaColourKey "SeaBattleBackground1.png"  320 160
        image MagentaColourKey "SeaBattleBackground2.png"  320 160
        image MagentaColourKey "SeaBattleBackground3.png"  320 160
        image MagentaColourKey "EnemyShip0.png"             12  15
        image MagentaColourKey "EnemyShip1.png"             32  16
        image MagentaColourKey "EnemyShip2.png"             67  16
        image MagentaColourKey "EnemyShip3.png"             12  15
        image MagentaColourKey "EnemyShip4.png"             34  16
        
        image NoColourKey      "TankBattleMissile.png"         12   1
        image NoColourKey      "TileBarricade.png"             16  16
        image NoColourKey      "TileBridge.png"                16  16
        image NoColourKey      "TileMine.png"                  16  16
        image NoColourKey      "TileSand.png"                  16  16
        image NoColourKey      "TileTower.png"                 16  16
        image NoColourKey      "TileWater.png"                 16  16
        image MagentaColourKey "EnemyTankGun1.png"             24  12
        image MagentaColourKey "TankFacingLeft0.png"           24  12
        image MagentaColourKey "TankFacingUpLeft0.png"         24  14
        image MagentaColourKey "TankFacingDownLeft0.png"       24  14
        image MagentaColourKey "TankFacingLeft1.png"           24  12
        image MagentaColourKey "TankFacingUpLeft1.png"         24  14
        image MagentaColourKey "TankFacingDownLeft1.png"       24  14
        
        image NoColourKey      "FinalBossBackground.png"      320 160
        image MagentaColourKey "FinalBoss0.png"               207  43
        image MagentaColourKey "FinalBoss1.png"               152  42
        image MagentaColourKey "FinalBoss2.png"                94  42
        image MagentaColourKey "FinalBoss3.png"                94  42
        image MagentaColourKey "FinalBoss4.png"                94  42
        image MagentaColourKey "FinalBoss5.png"                94  42
        image MagentaColourKey "FinalBossSurrender0.png"       16  35
        image MagentaColourKey "FinalBossSurrender1.png"       16  35
        image MagentaColourKey "FinalBossActiveTarget.png"      4   4
        image MagentaColourKey "FinalBossTarget.png"            4   4
        
        image MagentaColourKey "ShipGunNozzle.png"             16   8
        image NoColourKey      "ShipGunStem.png"               12   1
        image MagentaColourKey "ShipGunBullet.png"             16   8
        image NoColourKey      "IntermissionBackground.png"   320 200
        
        image NoColourKey      "VictoryScreen.png"            320 200
        image MagentaColourKey "ColonelReceivingMedal.png"     70 130
        image MagentaColourKey "ColonelSaluting.png"           72 130
        image MagentaColourKey "ColonelStanding.png"           50 130
        image MagentaColourKey "SoldierSaluting.png"           54 120
        image MagentaColourKey "SoldierStanding.png"           54 120
        image MagentaColourKey "Medal.png"                      7  16
        image MagentaColourKey "PresidentPresentingMedal.png"  66 130
        image MagentaColourKey "PresidentSaluting.png"         62 130
        image MagentaColourKey "PresidentStanding.png"         39 130
    ]


