module FortAssaultResourceFiles

open ImagesAndFonts
open Sounds

let private image colourKey fileName =
    {
        RequestedImageTransparency = colourKey
        RequestedImageFileName     = fileName
    }

let private font fileName charWidth =
    {
        RequestedFontImage     = image MagentaColourKeyImage fileName
        RequestedFontCharWidth = charWidth
    }

let private sound fileName =
    {
        RequestedSoundFileName = fileName
    }
    
let FortAssaultFontResourceImages =
    [
        font "MilitaryFontRed.png"     8
        font "MilitaryFontYellow.png"  8
        font "MilitaryFontBlue.png"    8
        font "MilitaryFontBlack.png"   8
        font "MilitaryFontSymbols.png" 8
    ]

let FortAssaultResourceImages =
    [
        image OpaqueImage           "CliffsTile.png"          
        image MagentaColourKeyImage "Title.png"          
        
        image OpaqueImage           "Map.png"                
        image MagentaColourKeyImage "AlliedFleetSymbol.png"
        image MagentaColourKeyImage "EnemyFleetSymbol.png" 
        
        image OpaqueImage           "SecretPassage.png"      
        image MagentaColourKeyImage "Mine.png"             
        image MagentaColourKeyImage "Ship0.png"            
        image MagentaColourKeyImage "Ship1.png"            
        image MagentaColourKeyImage "Ship2.png"            
        image MagentaColourKeyImage "Ship3.png"            
        image MagentaColourKeyImage "Ship4.png"            
        image MagentaColourKeyImage "Torpedo0.png"         
        image MagentaColourKeyImage "Torpedo22degrees.png" 
        image MagentaColourKeyImage "ShipExplode0.png"     
        image MagentaColourKeyImage "ShipExplode1.png"     
        image MagentaColourKeyImage "ShipExplode2.png"     
        image MagentaColourKeyImage "ShipExplode3.png"     
        
        image MagentaColourKeyImage "Plane0.png"           
        image MagentaColourKeyImage "Plane1.png"           
        image MagentaColourKeyImage "Plane2.png"           
        image MagentaColourKeyImage "Plane3.png"           
        image MagentaColourKeyImage "Plane4.png"           
        image MagentaColourKeyImage "Plane4BankingLeft.png"
        image MagentaColourKeyImage "Plane4BankingRight.png"
        image MagentaColourKeyImage "Plane5.png"            
        image MagentaColourKeyImage "Plane5BankingLeft.png" 
        image MagentaColourKeyImage "Plane5BankingRight.png"
        image MagentaColourKeyImage "Plane6.png"            
        image MagentaColourKeyImage "Plane6BankingLeft.png" 
        image MagentaColourKeyImage "Plane6BankingRight.png"
        image MagentaColourKeyImage "PlaneExplode0.png"     
        image MagentaColourKeyImage "PlaneExplode1.png"     
        image MagentaColourKeyImage "PlaneExplode2.png"     
        image MagentaColourKeyImage "Splash0.png"           
        image MagentaColourKeyImage "PlaneBomb0.png"        
        image MagentaColourKeyImage "PlaneBomb1.png"        
        image MagentaColourKeyImage "PlaneBomb2.png"        
        image MagentaColourKeyImage "PlaneBomb3.png"        
        image MagentaColourKeyImage "PlaneBomb4.png"        
        
        image MagentaColourKeyImage "SeaBattleBackground0.png"
        image MagentaColourKeyImage "SeaBattleBackground1.png"
        image MagentaColourKeyImage "SeaBattleBackground2.png"
        image MagentaColourKeyImage "SeaBattleBackground3.png"
        image MagentaColourKeyImage "EnemyShip0.png"          
        image MagentaColourKeyImage "EnemyShip1.png"          
        image MagentaColourKeyImage "EnemyShip2.png"          
        image MagentaColourKeyImage "EnemyShip3.png"          
        image MagentaColourKeyImage "EnemyShip4.png"          
        
        image OpaqueImage           "TankBattleMissile.png"        
        image OpaqueImage           "TileBarricade.png"            
        image OpaqueImage           "TileBridge.png"               
        image OpaqueImage           "TileMine.png"                 
        image OpaqueImage           "TileSand.png"                 
        image OpaqueImage           "TileTower.png"                
        image OpaqueImage           "TileWater.png"                
        image MagentaColourKeyImage "EnemyTankGun1.png"       
        image MagentaColourKeyImage "TankFacingLeft0.png"     
        image MagentaColourKeyImage "TankFacingUpLeft0.png"   
        image MagentaColourKeyImage "TankFacingDownLeft0.png" 
        image MagentaColourKeyImage "TankFacingLeft1.png"     
        image MagentaColourKeyImage "TankFacingUpLeft1.png"   
        image MagentaColourKeyImage "TankFacingDownLeft1.png" 
        
        image OpaqueImage           "FinalBossBackground.png"       
        image MagentaColourKeyImage "FinalBoss0.png"           
        image MagentaColourKeyImage "FinalBoss1.png"           
        image MagentaColourKeyImage "FinalBoss2.png"           
        image MagentaColourKeyImage "FinalBoss3.png"           
        image MagentaColourKeyImage "FinalBoss4.png"           
        image MagentaColourKeyImage "FinalBoss5.png"           
        image MagentaColourKeyImage "FinalBossSurrender0.png"  
        image MagentaColourKeyImage "FinalBossSurrender1.png"  
        image MagentaColourKeyImage "FinalBossActiveTarget.png"
        image MagentaColourKeyImage "FinalBossTarget.png"      
        
        image MagentaColourKeyImage "ShipGunNozzle.png"        
        image OpaqueImage           "ShipGunStem.png"               
        image MagentaColourKeyImage "ShipGunBullet.png"        
        image OpaqueImage           "IntermissionBackground.png"   
        
        image OpaqueImage           "VictoryScreen.png"                  
        image MagentaColourKeyImage "ColonelReceivingMedal.png"     
        image MagentaColourKeyImage "ColonelSaluting.png"           
        image MagentaColourKeyImage "ColonelStanding.png"           
        image MagentaColourKeyImage "SoldierSaluting.png"           
        image MagentaColourKeyImage "SoldierStanding.png"           
        image MagentaColourKeyImage "Medal.png"                     
        image MagentaColourKeyImage "PresidentPresentingMedal.png"  
        image MagentaColourKeyImage "PresidentSaluting.png"         
        image MagentaColourKeyImage "PresidentStanding.png"         
    ]

let FortAssaultResourceSounds : RequestedSound list =
    [
    ]
