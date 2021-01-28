module PacmanWeb

open WebGameFramework

open PacmanResourceFiles
open Storyboard
open Input



// ------------------------------------------------------------------------------------------------------------
//  BOOT
// ------------------------------------------------------------------------------------------------------------

let WebMain () =

    let pacmanKeysNeeded =
        [
            WebBrowserKeyCode 37
            WebBrowserKeyCode 39
            WebBrowserKeyCode 38
            WebBrowserKeyCode 40
            WebBrowserKeyCode 90
        ]

    let stubGlobalStateConstructor () = Ok 1  // Feature not used by Pacman.

    LoadResourceFilesThenDo 
        PacmanResourceImages 
        PacmanFontResourceImages 
        PacmanResourceSounds
        (FrameworkWebMain
            pacmanKeysNeeded
            stubGlobalStateConstructor 
            NewPacmanStoryboard)



// ------------------------------------------------------------------------------------------------------------
//  Javascript-land runs this script right away:
// ------------------------------------------------------------------------------------------------------------

WebMain ()

