module DrawingBarChart

open ResourceFileMetadata
open Geometry
open DrawingFunctions

type BarChartStyle =
    {
        BackgroundFont      : FontID
        BackgroundCharIndex : uint32

        ForegroundFont      : FontID
        ForegroundCharIndex : uint32

        SpacingX            : int<epx>
        SpacingY            : int<epx>
    }


type BarChartStat =
    {
        BarTotal : uint32
        BarValue : uint32
    }


let Bar render (x:int<epx>) (y:int<epx>) barStyle (chart:BarChartStat) =

    let t = chart.BarTotal
    let v = min t chart.BarValue

    let (x,y) =
        DrawRepeatedChar 
            render 
            barStyle.BackgroundFont 
            barStyle.SpacingX
            barStyle.SpacingY
            barStyle.BackgroundCharIndex
            x
            y
            v

    DrawRepeatedChar 
        render
        barStyle.ForegroundFont
        barStyle.SpacingX
        barStyle.SpacingY
        barStyle.ForegroundCharIndex
        x
        y
        (t - v)
            |> ignore


