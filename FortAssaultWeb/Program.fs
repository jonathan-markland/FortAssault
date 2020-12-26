module FortAssaultWeb

open WebGameFramework

open FortAssaultResourceFiles
open Storyboard
open Input



// ------------------------------------------------------------------------------------------------------------
//  BOOT
// ------------------------------------------------------------------------------------------------------------

let WebMain () =

    let fortAssaultKeysNeeded =
        [
            WebBrowserKeyCode 37
            WebBrowserKeyCode 39
            WebBrowserKeyCode 38
            WebBrowserKeyCode 40
            WebBrowserKeyCode 90
        ]

    let stubGlobalStateConstructor () = Ok 1  // Feature not used by Fort Assault.

    LoadResourceFilesThenDo 
        FortAssaultResourceImages 
        FortAssaultFontResourceImages 
        FortAssaultResourceSounds
        (FrameworkWebMain
            fortAssaultKeysNeeded
            stubGlobalStateConstructor 
            NewFortAssaultStoryboard)




// ------------------------------------------------------------------------------------------------------------
//  Javascript-land runs this script right away:
// ------------------------------------------------------------------------------------------------------------

WebMain ()

