module SharedDrawing

open Geometry
open DrawingCommands
open DrawingCommandsEx
open ImagesAndFonts

type BarChartStat =
    {
        BlueCount:     uint32
        RedCount:      uint32
        RedCharIndex:  uint32
        BlueCharIndex: uint32
        CharSpacing:   uint32
    }

let Bar render (x:int<wu>) (y:int<wu>) (chart:BarChartStat) =

    let mutable x' = x
    let spacing = (int chart.CharSpacing) |> IntToIntWu

    let appendRow numRepeats idx fontVisual = 
        let mutable count = numRepeats
        while count > 0u do
            render (DrawCharImageWithTopLeftAt(x', y, idx, fontVisual))
            count <- count - 1u
            x' <- x' + spacing

    appendRow chart.BlueCount chart.BlueCharIndex SymbolFontID
    appendRow chart.RedCount  chart.RedCharIndex  SymbolFontID

let ScoreboardArea render contentHeight =
    Rectangle render 0<wu> contentHeight ScreenWidthInt (ScreenHeightInt - contentHeight) (SolidColour(0000000u))

