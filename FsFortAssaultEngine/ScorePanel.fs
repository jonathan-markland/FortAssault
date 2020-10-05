module ScorePanel

open Angle
open ScoreHiScore
open DrawingBarChart
open ResourceIDs
open DrawingFunctions
open ImagesAndFonts
open Geometry

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Style definitions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ShipBarChartStyle =
    {
        BackgroundFont      = SymbolFontID
        BackgroundCharIndex = 4u
        ForegroundFont      = SymbolFontID
        ForegroundCharIndex = 2u
        SpacingX            = 8<epx>
        SpacingY            = 0<epx>
    }

let TanksBarChartStyle =
    {
        BackgroundFont      = SymbolFontID
        BackgroundCharIndex = 3u
        ForegroundFont      = SymbolFontID
        ForegroundCharIndex = 1u
        SpacingX            = 8<epx>
        SpacingY            = 0<epx>
    }

let DamageBarChartStyle =
    {
        BackgroundFont      = SymbolFontID
        BackgroundCharIndex = 5u
        ForegroundFont      = SymbolFontID
        ForegroundCharIndex = 6u
        SpacingX            = 8<epx>
        SpacingY            = 0<epx>
    }

let IntelligenceBarChartStyle =
    {
        BackgroundFont      = SymbolFontID
        BackgroundCharIndex = 7u
        ForegroundFont      = SymbolFontID
        ForegroundCharIndex = 7u
        SpacingX            = 8<epx>
        SpacingY            = 0<epx>
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Score panel
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

[<Struct>]
type ScorePanel =
    {
        ScoreAndHiScore:  ScoreAndHiScore
        ShipsPending:     uint32
        ShipsThrough:     uint32
        Tanks:            uint32
        Damage:           uint32
        MaxDamage:        uint32
        PlaneIntel:       uint32 option
        Elevation:        float32<degrees>
    }

let ShipBarChart panel =
    {
        BarTotal = panel.ShipsThrough + panel.ShipsPending
        BarValue = panel.ShipsThrough
    }

let TanksBarChart panel =
    {
        BarTotal = panel.Tanks
        BarValue = panel.Tanks
    }

let DamageBarChart panel =
    {
        BarTotal = panel.MaxDamage
        BarValue = panel.Damage
    }

let IntelligenceBarChart panel v =
    {
        BarTotal = v
        BarValue = v
    }



let DrawScorePanel render y (panel:ScorePanel) =

    let c1x = 4<epx>
    let c2x = c1x + 64<epx>
    let c3x = ScreenWidthInt / 2
    let y   = y + 4<epx>

    Text render YellowFontID LeftAlign TopAlign c1x y "SCORE"
    Num  render RedFontID LeftAlign TopAlign c2x y (panel.ScoreAndHiScore.Score)

    let shy = y + 12<epx>
    Text render YellowFontID LeftAlign TopAlign c1x shy "SHIPS"
    Bar  render c2x shy ShipBarChartStyle (panel |> ShipBarChart)

    if panel.MaxDamage > 0u then
        let dmy = y + 24<epx>
        Text render YellowFontID LeftAlign TopAlign c1x dmy "DAMAGE"
        Bar  render c2x dmy DamageBarChartStyle (panel |> DamageBarChart)

    match panel.PlaneIntel with
        | Some planeCount ->
            Text render YellowFontID LeftAlign TopAlign c3x y "INTEL"
            Bar  render (c3x + 48<epx>) y IntelligenceBarChartStyle (IntelligenceBarChart panel planeCount)
        | None ->
            ()

    let tay = y + 12<epx>
    Text render YellowFontID LeftAlign TopAlign c3x tay "TANKS"
    Bar  render (c3x + 48<epx>) tay TanksBarChartStyle (panel |> TanksBarChart)

    let ely = y + 24<epx>
    Text render YellowFontID LeftAlign TopAlign c3x ely "ELEVATION"
    Flo  render BlueFontID LeftAlign TopAlign (c3x + 96<epx>) ely (float32 panel.Elevation)



let DrawTankBattleScorePanel render y (score:uint32) (numTanks:uint32) =

    let x = ScreenWidthInt / 2
    let y = y + 2<epx>

    let message = sprintf "SCORE  %d     TANKS  %d" score numTanks
    Text render BlackFontID CentreAlign TopAlign x y message
