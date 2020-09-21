module ScorePanel

open Angle
open ScoreHiScore
open DrawingBarChart
open ImagesAndFonts
open DrawingCommandsEx
open FontAlignment
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

let AmmoBarChartStyle =
    {
        BackgroundFont      = SymbolFontID
        BackgroundCharIndex = 0u
        ForegroundFont      = SymbolFontID
        ForegroundCharIndex = 0u
        SpacingX            = 16<epx>
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
        Ammunition:       uint32
        Elevation:        float32<degrees>
    }

let ShipBarChart panel =
    {
        BarTotal = panel.ShipsThrough + panel.ShipsPending
        BarValue = panel.ShipsPending
    }

let TanksBarChart panel =
    {
        BarTotal = panel.Tanks
        BarValue = panel.Tanks
    }

let AmmoBarChart panel =
    {
        BarTotal = panel.Ammunition
        BarValue = panel.Ammunition
    }

let DamageBarChart panel =
    {
        BarTotal = panel.MaxDamage
        BarValue = panel.Damage
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

    // TODO: The ammunition concept was limiting firing repeats, but it's not
    //       used, in favour of having to wait.  Need to introduce a use for
    //       this perhaps in future.
    //
    // Text render YellowFontVisual LeftAlign TopAlign c3x y "AMMUNITION"
    // Bar  render c3x (y + 8<wu>) (panel |> AmmoBarChart)

    let tay = y + 16<epx>
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
