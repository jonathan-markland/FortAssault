module ImagesAndFonts

open Geometry
open ResourceFileMetadata
open DrawingShapes

// This game uses a 320*200 engine-coordinate (epx) space:

let ScreenWidthInt  = 320<epx>  // Must be the same as below!
let ScreenHeightInt = 200<epx>  // Must be the same as below!

let ScreenWidth  = 320.0F<epx>
let ScreenHeight = 200.0F<epx>

// Fonts (all screens):

let RedFontID                   = FontID(0)
let YellowFontID                = FontID(1)
let BlueFontID                  = FontID(2)
let BlackFontID                 = FontID(3)
let SymbolFontID                = FontID(4)

// TODO: The client should check that its dimensions (relatively) matches these here.

let CliffsTileImageID           = ImageID(0)
let ImageTitle                  = ImageID(1)

// Map screens:

let ImageMap                    = ImageID(2)
let ImageAlliedFleetSymbol      = ImageID(3)
let ImageEnemyFleetSymbol       = ImageID(4)

// Secret passage screen:

let ImageSecretPassage          = ImageID(5) 
let ImageMine                   = ImageID(6) 
let ImageShip0                  = ImageID(7) 
let ImageShip1                  = ImageID(8) 
let ImageShip2                  = ImageID(9) 
let ImageShip3                  = ImageID(10)
let ImageShip4                  = ImageID(11)
let ImageTorpedo0               = ImageID(12)
let ImageTorpedo22Degrees       = ImageID(13)
let ImageShipExplode0           = ImageID(14)
let ImageShipExplode1           = ImageID(15)
let ImageShipExplode2           = ImageID(16)
let ImageShipExplode3           = ImageID(17)

// Air battle screen:

let ImagePlane0                 = ImageID(18)
let ImagePlane1                 = ImageID(19)
let ImagePlane2                 = ImageID(20)
let ImagePlane3                 = ImageID(21)
let ImagePlane4                 = ImageID(22)
let ImagePlane4BankingLeft      = ImageID(23)
let ImagePlane4BankingRight     = ImageID(24)
let ImagePlane5                 = ImageID(25)
let ImagePlane5BankingLeft      = ImageID(26)
let ImagePlane5BankingRight     = ImageID(27)
let ImagePlane6                 = ImageID(28)
let ImagePlane6BankingLeft      = ImageID(29)
let ImagePlane6BankingRight     = ImageID(30)
let ImagePlaneExplode0          = ImageID(31)
let ImagePlaneExplode1          = ImageID(32)
let ImagePlaneExplode2          = ImageID(33)
let ImageSplash0                = ImageID(34)
let ImagePlaneBomb0             = ImageID(35)
let ImagePlaneBomb1             = ImageID(36)
let ImagePlaneBomb2             = ImageID(37)
let ImagePlaneBomb3             = ImageID(38)
let ImagePlaneBomb4             = ImageID(39)

// Sea and Air battle screens:

let ImageSeaBattleBackground0   = ImageID(40)
let ImageSeaBattleBackground1   = ImageID(41)
let ImageSeaBattleBackground2   = ImageID(42)
let ImageSeaBattleBackground3   = ImageID(43)
let ImageEnemyShip0             = ImageID(44)
let ImageEnemyShip1             = ImageID(45)
let ImageEnemyShip2             = ImageID(46)
let ImageEnemyShip3             = ImageID(47)
let ImageEnemyShip4             = ImageID(48)

// Tank battle screen:

let ImageTankBattleMissile      = ImageID(49)
let ImageTileBarricade          = ImageID(50)
let ImageTileBridge             = ImageID(51)
let ImageTileMine               = ImageID(52)
let ImageTileSand               = ImageID(53)
let ImageTileTower              = ImageID(54)
let ImageTileWater              = ImageID(55)
let ImageEnemyTankGun1          = ImageID(56)
let ImageTankFacingLeft0        = ImageID(57)
let ImageTankFacingUpLeft0      = ImageID(58)
let ImageTankFacingDownLeft0    = ImageID(59)
let ImageTankFacingLeft1        = ImageID(60)
let ImageTankFacingUpLeft1      = ImageID(61)
let ImageTankFacingDownLeft1    = ImageID(62)

// Final boss screen:

let ImageFinalBossBackground    = ImageID(63)
let ImageFinalBoss0             = ImageID(64)
let ImageFinalBoss1             = ImageID(65)
let ImageFinalBoss2             = ImageID(66)
let ImageFinalBoss3             = ImageID(67)
let ImageFinalBoss4             = ImageID(68)
let ImageFinalBoss5             = ImageID(69)
let ImageFinalBossSurrender0    = ImageID(70)
let ImageFinalBossSurrender1    = ImageID(71)
let ImageFinalBossActiveTarget  = ImageID(72)
let ImageFinalBossTarget        = ImageID(73)

// Shared images

let ShipGunNozzleImageID        = ImageID(74)
let ShipGunStemImageID          = ImageID(75)
let ShipGunBulletImageID        = ImageID(76)
let ImageIntermissionBackground = ImageID(77)

// Victory screen:

let VictoryScreenImageID          = ImageID(78)
let ImageColonelReceivingMedal    = ImageID(79)
let ImageColonelSaluting          = ImageID(80)
let ImageColonelStanding          = ImageID(81)
let ImageSoldierSaluting          = ImageID(82)
let ImageSoldierStanding          = ImageID(83)
let ImageMedal                    = ImageID(84)
let ImagePresidentPresentingMedal = ImageID(85)
let ImagePresidentSaluting        = ImageID(86)
let ImagePresidentStanding        = ImageID(87)

// Shared gun sizing:

let ShipGunStemImageWidth     = 12<epx>
let ShipGunStemImageHeight    =  1<epx>
let ShipGunNozzleImageWidth   = 16<epx>
let ShipGunNozzleImageWidthF  = 16.0F<epx>
let ShipGunNozzleImageHeight  =  8<epx>
let ShipGunBulletImageWidth   = 16<epx>
