module MissionIIResourceFiles

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


let MissionIIFontResourceImages =
    [
        font "Font.png" 6
    ]

let MissionIIResourceImages =
    [
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
        image OpaqueImage           "WallBrick_4.png"        
        image OpaqueImage           "WallBrick_5.png"        
        image OpaqueImage           "WallBrick_6.png"        
        image OpaqueImage           "WallBrick_7.png"        
        image OpaqueImage           "WallBrick_8.png"        
        image OpaqueImage           "WallElectric.png"       
        image OpaqueImage           "WallOutline_1.png"      
        image OpaqueImage           "WallOutline_2.png"      
        image OpaqueImage           "WallOutline_3.png"      
    ]

let MissionIIResourceSounds =
    [
        sound "BonusSound.ogg" 
        sound "DroidFiringSound.ogg" 
        sound "DuoBonusSound.ogg" 
        sound "ElectrocutionSound.ogg" 
        sound "EnteringLevelSound.ogg" 
        sound "ExplosionSound.ogg" 
        sound "ExtraLifeSound.ogg" 
        sound "Footstep1Sound.ogg" 
        sound "Footstep2Sound.ogg" 
        sound "GameOverSound_1.ogg"
        sound "GameOverSound_2.ogg" 
        sound "GameOverSound_3.ogg" 
        sound "GameOverSound_4.ogg" 
        sound "GhostAppearingSound.ogg" 
        sound "IntroSound.ogg" 
        sound "InvincibilityAmuletSound.ogg" 
        sound "LevelExitActivatedSound.ogg" 
        sound "ManFiringSound.ogg" 
        sound "ManGruntSound_1.ogg" 
        sound "ManGruntSound_2.ogg" 
        sound "PauseModeSound.ogg" 
        sound "PickUpObjectSound.ogg" 
        sound "StunGhostSound.ogg" 
    ]
