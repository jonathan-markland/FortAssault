module FortAssaultWeb

open WebGameFramework

open FortAssaultImageFiles
open TankMapFileLoader
open Storyboard
open FortAssaultGlobalState
open FortAssaultGameResources
open Input



// ------------------------------------------------------------------------------------------------------------
//  BOOT
// ------------------------------------------------------------------------------------------------------------

let WebMain () =

    // let fortAssaultStaticDataConstructor () = 
    //    LoadTankBattleSequences () 
    //        |> Result.map (fun tankMapsList -> { 1TankMapsList = tankMapsList })

    let fortAssaultKeysNeeded =
        [
            WebBrowserKeyCode 37
            WebBrowserKeyCode 39
            WebBrowserKeyCode 38
            WebBrowserKeyCode 40
            WebBrowserKeyCode 90
        ]

    LoadResourceFilesThenDo 
        FortAssaultResourceImages 
        FortAssaultFontResourceImages 
        (FrameworkWebMain
            fortAssaultKeysNeeded
            FortAssaultGlobalStateConstructor 
            NewFortAssaultStoryboard)




// ------------------------------------------------------------------------------------------------------------
//  Javascript land runs this script right away:
// ------------------------------------------------------------------------------------------------------------

WebMain ()

