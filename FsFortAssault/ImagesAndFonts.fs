module ImagesAndFonts

open Geometry
open DrawingCommands
open DrawingCommandsEx

// This game uses a 320*200 world-coordinate (wu) space:

let ScreenWidthInt  = 320<epx>  // Must be the same as below!
let ScreenHeightInt = 200<epx>  // Must be the same as below!

let ScreenWidth  = 320.0F<epx>
let ScreenHeight = 200.0F<epx>

// Fonts (all screens):

let RedFontID    = FontID(0)
let YellowFontID = FontID(1)
let BlueFontID   = FontID(2)
let BlackFontID  = FontID(3)
let SymbolFontID = FontID(4)

// TODO: The client should check that its dimensions (relatively) matches these here.

let CliffsTileImageID           = ImageID(0)
let ImageTitle                  = { ImageID = ImageID(1) ; ImageWidth = 297.0F<epx> ; ImageHeight = 20.0F<epx>  }

// Map screens:

let ImageMap                    = { ImageID = ImageID(2) ; ImageWidth = 320.0F<epx> ; ImageHeight = 160.0F<epx> }
let ImageAlliedFleetSymbol      = { ImageID = ImageID(3) ; ImageWidth =  21.0F<epx> ; ImageHeight =  21.0F<epx> }
let ImageEnemyFleetSymbol       = { ImageID = ImageID(4) ; ImageWidth =  20.0F<epx> ; ImageHeight =  20.0F<epx> }

// Secret passage screen:

let ImageSecretPassage          = { ImageID = ImageID(5)  ; ImageWidth = 320.0F<epx> ; ImageHeight = 160.0F<epx> }
let ImageMine                   = { ImageID = ImageID(6)  ; ImageWidth = 6.0F<epx>   ; ImageHeight = 8.0F<epx>   }
let ImageShip0                  = { ImageID = ImageID(7)  ; ImageWidth = 40.0F<epx>  ; ImageHeight = 13.0F<epx>  }
let ImageShip1                  = { ImageID = ImageID(8)  ; ImageWidth = 34.0F<epx>  ; ImageHeight = 18.0F<epx>  }
let ImageShip2                  = { ImageID = ImageID(9)  ; ImageWidth = 31.0F<epx>  ; ImageHeight = 21.0F<epx>  }
let ImageShip3                  = { ImageID = ImageID(10) ; ImageWidth = 26.0F<epx>  ; ImageHeight = 26.0F<epx>  }
let ImageShip4                  = { ImageID = ImageID(11) ; ImageWidth = 12.0F<epx>  ; ImageHeight = 36.0F<epx>  }
let ImageTorpedo0               = { ImageID = ImageID(12) ; ImageWidth = 8.0F<epx>   ; ImageHeight = 2.0F<epx>   }
let ImageTorpedo22Degrees       = { ImageID = ImageID(13) ; ImageWidth = 8.0F<epx>   ; ImageHeight = 5.0F<epx>   }
let ImageShipExplode0           = { ImageID = ImageID(14) ; ImageWidth = 25.0F<epx>  ; ImageHeight = 25.0F<epx>  }
let ImageShipExplode1           = { ImageID = ImageID(15) ; ImageWidth = 50.0F<epx>  ; ImageHeight = 37.0F<epx>  }
let ImageShipExplode2           = { ImageID = ImageID(16) ; ImageWidth = 50.0F<epx>  ; ImageHeight = 49.0F<epx>  }
let ImageShipExplode3           = { ImageID = ImageID(17) ; ImageWidth = 50.0F<epx>  ; ImageHeight = 49.0F<epx>  }

// Air battle screen:

let ImagePlane0                 = { ImageID = ImageID(18) ; ImageWidth =  6.0F<epx>  ; ImageHeight =  2.0F<epx> }
let ImagePlane1                 = { ImageID = ImageID(19) ; ImageWidth = 18.0F<epx>  ; ImageHeight =  6.0F<epx> }
let ImagePlane2                 = { ImageID = ImageID(20) ; ImageWidth = 31.0F<epx>  ; ImageHeight = 10.0F<epx> }
let ImagePlane3                 = { ImageID = ImageID(21) ; ImageWidth = 49.0F<epx>  ; ImageHeight = 19.0F<epx> }
let ImagePlane4                 = { ImageID = ImageID(22) ; ImageWidth = 72.0F<epx>  ; ImageHeight = 26.0F<epx> }
let ImagePlane4BankingLeft      = { ImageID = ImageID(23) ; ImageWidth = 62.0F<epx>  ; ImageHeight = 21.0F<epx> }
let ImagePlane4BankingRight     = { ImageID = ImageID(24) ; ImageWidth = 62.0F<epx>  ; ImageHeight = 21.0F<epx> }
let ImagePlane5                 = { ImageID = ImageID(25) ; ImageWidth = 73.0F<epx>  ; ImageHeight = 22.0F<epx> }
let ImagePlane5BankingLeft      = { ImageID = ImageID(26) ; ImageWidth = 66.0F<epx>  ; ImageHeight = 20.0F<epx> }
let ImagePlane5BankingRight     = { ImageID = ImageID(27) ; ImageWidth = 66.0F<epx>  ; ImageHeight = 20.0F<epx> }
let ImagePlane6                 = { ImageID = ImageID(28) ; ImageWidth = 71.0F<epx>  ; ImageHeight = 27.0F<epx> }
let ImagePlane6BankingLeft      = { ImageID = ImageID(29) ; ImageWidth = 69.0F<epx>  ; ImageHeight = 26.0F<epx> }
let ImagePlane6BankingRight     = { ImageID = ImageID(30) ; ImageWidth = 69.0F<epx>  ; ImageHeight = 26.0F<epx> }
let ImagePlaneExplode0          = { ImageID = ImageID(31) ; ImageWidth = 45.0F<epx>  ; ImageHeight = 23.0F<epx> }
let ImagePlaneExplode1          = { ImageID = ImageID(32) ; ImageWidth = 51.0F<epx>  ; ImageHeight = 26.0F<epx> }
let ImagePlaneExplode2          = { ImageID = ImageID(33) ; ImageWidth = 35.0F<epx>  ; ImageHeight = 19.0F<epx> }
let ImageSplash0                = { ImageID = ImageID(34) ; ImageWidth = 12.0F<epx>  ; ImageHeight =  9.0F<epx> }
let ImagePlaneBomb0             = { ImageID = ImageID(35) ; ImageWidth =  2.0F<epx>  ; ImageHeight =  2.0F<epx> }
let ImagePlaneBomb1             = { ImageID = ImageID(36) ; ImageWidth =  4.0F<epx>  ; ImageHeight =  4.0F<epx> }
let ImagePlaneBomb2             = { ImageID = ImageID(37) ; ImageWidth =  6.0F<epx>  ; ImageHeight =  6.0F<epx> }
let ImagePlaneBomb3             = { ImageID = ImageID(38) ; ImageWidth = 10.0F<epx>  ; ImageHeight = 10.0F<epx> }
let ImagePlaneBomb4             = { ImageID = ImageID(39) ; ImageWidth = 12.0F<epx>  ; ImageHeight = 12.0F<epx> }

// Sea and Air battle screens:

let ImageSeaBattleBackground0   = { ImageID = ImageID(40) ; ImageWidth = 320.0F<epx>  ; ImageHeight = 160.0F<epx> }
let ImageSeaBattleBackground1   = { ImageID = ImageID(41) ; ImageWidth = 320.0F<epx>  ; ImageHeight = 160.0F<epx> }
let ImageSeaBattleBackground2   = { ImageID = ImageID(42) ; ImageWidth = 320.0F<epx>  ; ImageHeight = 160.0F<epx> }
let ImageSeaBattleBackground3   = { ImageID = ImageID(43) ; ImageWidth = 320.0F<epx>  ; ImageHeight = 160.0F<epx> }
let ImageEnemyShip0             = { ImageID = ImageID(44) ; ImageWidth = 12.0F<epx>  ; ImageHeight = 15.0F<epx> }
let ImageEnemyShip1             = { ImageID = ImageID(45) ; ImageWidth = 32.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageEnemyShip2             = { ImageID = ImageID(46) ; ImageWidth = 67.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageEnemyShip3             = { ImageID = ImageID(47) ; ImageWidth = 12.0F<epx>  ; ImageHeight = 15.0F<epx> }
let ImageEnemyShip4             = { ImageID = ImageID(48) ; ImageWidth = 34.0F<epx>  ; ImageHeight = 16.0F<epx> }

// Tank battle screen:

let ImageTankBattleMissile      = { ImageID = ImageID(49) ; ImageWidth = 12.0F<epx>  ; ImageHeight =  1.0F<epx> }
let ImageTileBarricade          = { ImageID = ImageID(50) ; ImageWidth = 16.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageTileBridge             = { ImageID = ImageID(51) ; ImageWidth = 16.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageTileMine               = { ImageID = ImageID(52) ; ImageWidth = 16.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageTileSand               = { ImageID = ImageID(53) ; ImageWidth = 16.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageTileTower              = { ImageID = ImageID(54) ; ImageWidth = 16.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageTileWater              = { ImageID = ImageID(55) ; ImageWidth = 16.0F<epx>  ; ImageHeight = 16.0F<epx> }
let ImageEnemyTankGun1          = { ImageID = ImageID(56) ; ImageWidth = 24.0F<epx>  ; ImageHeight = 12.0F<epx> }
let ImageTankFacingLeft0        = { ImageID = ImageID(57) ; ImageWidth = 24.0F<epx>  ; ImageHeight = 12.0F<epx> }
let ImageTankFacingUpLeft0      = { ImageID = ImageID(58) ; ImageWidth = 24.0F<epx>  ; ImageHeight = 14.0F<epx> }
let ImageTankFacingDownLeft0    = { ImageID = ImageID(59) ; ImageWidth = 24.0F<epx>  ; ImageHeight = 14.0F<epx> }
let ImageTankFacingLeft1        = { ImageID = ImageID(60) ; ImageWidth = 24.0F<epx>  ; ImageHeight = 12.0F<epx> }
let ImageTankFacingUpLeft1      = { ImageID = ImageID(61) ; ImageWidth = 24.0F<epx>  ; ImageHeight = 14.0F<epx> }
let ImageTankFacingDownLeft1    = { ImageID = ImageID(62) ; ImageWidth = 24.0F<epx>  ; ImageHeight = 14.0F<epx> }

// Final boss screen:

let ImageFinalBossBackground    = { ImageID = ImageID(63) ; ImageWidth = 320.0F<epx>  ; ImageHeight = 160.0F<epx> }
let ImageFinalBoss0             = { ImageID = ImageID(64) ; ImageWidth = 207.0F<epx>  ; ImageHeight = 43.0F<epx>  }
let ImageFinalBoss1             = { ImageID = ImageID(65) ; ImageWidth = 152.0F<epx>  ; ImageHeight = 42.0F<epx>  }
let ImageFinalBoss2             = { ImageID = ImageID(66) ; ImageWidth =  94.0F<epx>  ; ImageHeight = 42.0F<epx>  }
let ImageFinalBoss3             = { ImageID = ImageID(67) ; ImageWidth =  94.0F<epx>  ; ImageHeight = 42.0F<epx>  }
let ImageFinalBoss4             = { ImageID = ImageID(68) ; ImageWidth =  94.0F<epx>  ; ImageHeight = 42.0F<epx>  }
let ImageFinalBoss5             = { ImageID = ImageID(69) ; ImageWidth =  94.0F<epx>  ; ImageHeight = 42.0F<epx>  }
let ImageFinalBossSurrender0    = { ImageID = ImageID(70) ; ImageWidth =  16.0F<epx>  ; ImageHeight = 35.0F<epx>  }
let ImageFinalBossSurrender1    = { ImageID = ImageID(71) ; ImageWidth =  16.0F<epx>  ; ImageHeight = 35.0F<epx>  }
let ImageFinalBossActiveTarget  = { ImageID = ImageID(72) ; ImageWidth = 4.0F<epx>    ; ImageHeight = 4.0F<epx>   }
let ImageFinalBossTarget        = { ImageID = ImageID(73) ; ImageWidth = 4.0F<epx>    ; ImageHeight = 4.0F<epx>   }

// Shared images

let ShipGunNozzleImageID        = ImageID(74)
let ShipGunStemImageID          = ImageID(75)
let ShipGunBulletImageID        = ImageID(76)
let ImageIntermissionBackground = { ImageID = ImageID(77) ; ImageWidth = 320.0F<epx> ; ImageHeight = 200.0F<epx> }

// Victory screen:

let VictoryScreenImageID          = ImageID(78)
let ImageColonelReceivingMedal    = { ImageID = ImageID(79) ; ImageWidth = 70.0F<epx> ; ImageHeight = 130.0F<epx> }
let ImageColonelSaluting          = { ImageID = ImageID(80) ; ImageWidth = 72.0F<epx> ; ImageHeight = 130.0F<epx> }
let ImageColonelStanding          = { ImageID = ImageID(81) ; ImageWidth = 50.0F<epx> ; ImageHeight = 130.0F<epx> }
let ImageSoldierSaluting          = { ImageID = ImageID(82) ; ImageWidth = 54.0F<epx> ; ImageHeight = 120.0F<epx> }
let ImageSoldierStanding          = { ImageID = ImageID(83) ; ImageWidth = 54.0F<epx> ; ImageHeight = 120.0F<epx> }
let ImageMedal                    = { ImageID = ImageID(84) ; ImageWidth =  7.0F<epx> ; ImageHeight =  16.0F<epx> }
let ImagePresidentPresentingMedal = { ImageID = ImageID(85) ; ImageWidth = 66.0F<epx> ; ImageHeight = 130.0F<epx> }
let ImagePresidentSaluting        = { ImageID = ImageID(86) ; ImageWidth = 62.0F<epx> ; ImageHeight = 130.0F<epx> }
let ImagePresidentStanding        = { ImageID = ImageID(87) ; ImageWidth = 39.0F<epx> ; ImageHeight = 130.0F<epx> }

// Shared gun sizing:

let ShipGunStemImageWidth    = 12.0F<epx>
let ShipGunStemImageHeight   =  1.0F<epx>
let ShipGunNozzleImageWidth  = 16.0F<epx>
let ShipGunNozzleImageHeight =  8.0F<epx>
let ShipGunBulletImageWidth  = 16.0F<epx>
