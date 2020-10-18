module FortAssaultWeb

open WebGameFramework

open FortAssaultImageFiles
open TankMapFileLoader
open Storyboard
open FortAssaultGlobalState
open Input
open EngineEntryPoint



// ------------------------------------------------------------------------------------------------------------
//  BOOT
// ------------------------------------------------------------------------------------------------------------

let WebMain () =

    let fortAssaultStaticDataConstructor () = 
       LoadTankBattleSequences () 
           |> Result.map (fun tankMapsList -> { TankMapsList = tankMapsList })

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
            fortAssaultStaticDataConstructor 
            FortAssaultGlobalStateConstructor 
            NewFortAssaultStoryboard)




// ------------------------------------------------------------------------------------------------------------
//  Javascript land runs this script right away:
// ------------------------------------------------------------------------------------------------------------

WebMain ()

