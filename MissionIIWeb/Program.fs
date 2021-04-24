module MissionIIWeb

open WebGameFramework

open MissionIIResourceFiles
open Storyboard
open Input
open GamePlayScreenConstants
open Geometry



// ------------------------------------------------------------------------------------------------------------
//  BOOT
// ------------------------------------------------------------------------------------------------------------

let WebMain () =

    let missioniiKeysNeeded =
        [
            WebBrowserKeyCode 37
            WebBrowserKeyCode 39
            WebBrowserKeyCode 38
            WebBrowserKeyCode 40
            WebBrowserKeyCode 90
        ]

    let stubGlobalStateConstructor () = Ok 1  // Feature not used.

    InitWebFrameworkThenDo 
        (ScreenWidthInt |> IntEpxToInt)
        (ScreenHeightInt |> IntEpxToInt)
        MissionIIResourceImages 
        MissionIIFontResourceImages 
        MissionIIResourceSounds
        (FrameworkWebMain
            missioniiKeysNeeded
            stubGlobalStateConstructor 
            NewMissionIIStoryboard)



// ------------------------------------------------------------------------------------------------------------
//  Javascript-land runs this script right away:
// ------------------------------------------------------------------------------------------------------------

WebMain ()

