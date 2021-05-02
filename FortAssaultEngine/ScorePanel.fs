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

    // TODO:  This could have stored memoized strings, since they rarely change compared to the
    //        number of frames (garbage reduction).  Although, the screens would have to incorporate
    //        this into their data model to be worth it.  Furthermore, if we're doing that, the
    //        screens could have natively used this information, rather than duplicate it in their
    //        data models, perhaps.

    {
        ScoreAndHiScore:  ScoreAndHiScore
        ShipsPending:     uint32
        ShipsThrough:     uint32
        Tanks:            uint32
        Damage:           uint32
        MaxDamage:        uint32
        PlaneIntel:       uint32 option
        Elevation:        float<degrees>
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

    // Columns
    let c1x = 4<epx>
    let c2x = c1x + 64<epx>
    let c3x = ScreenWidthInt / 2
    let c4x = c3x + 48<epx>
    let c5x = c3x + 96<epx>

    // Rows
    let r1y = y + 4<epx>
    let r2y = r1y + 12<epx>
    let r3y = r1y + 24<epx>

    // Left column:

    Text render YellowFontID LeftAlign TopAlign c1x r1y "SCORE"
    Num  render RedFontID LeftAlign TopAlign c2x r1y (panel.ScoreAndHiScore.Score)

    Text render YellowFontID LeftAlign TopAlign c1x r2y "SHIPS"
    Bar  render c2x r2y ShipBarChartStyle (panel |> ShipBarChart)

    if panel.MaxDamage > 0u then
        Text render YellowFontID LeftAlign TopAlign c1x r3y "DAMAGE"
        Bar  render c2x r3y DamageBarChartStyle (panel |> DamageBarChart)

    // Right column:

    Text render YellowFontID LeftAlign TopAlign c3x r1y "TANKS"
    Bar  render c4x r1y TanksBarChartStyle (panel |> TanksBarChart)

    Text render YellowFontID LeftAlign TopAlign c3x r2y "ELEVATION"
    Flo  render BlueFontID LeftAlign TopAlign c5x r2y (float32 panel.Elevation)

    match panel.PlaneIntel with
        | Some planeCount ->
            Text render YellowFontID LeftAlign TopAlign c3x r3y "INTEL"
            Bar  render c4x r3y IntelligenceBarChartStyle (IntelligenceBarChart panel planeCount)
        | None ->
            ()

    #if SHORT_PLAYTHROUGH
    // As the score panel is usually drawn last, this should work:
    Text render RedFontID CentreAlign MiddleAlign 160<epx> 10<epx> "WARNING  SHORT PLAY VERSION"
    #endif



let DrawTankBattleScorePanel render y (score:uint32) (numTanks:uint32) =

    let x = ScreenWidthInt / 2
    let y = y + 2<epx>

    let message = sprintf "SCORE  %d     TANKS  %d" score numTanks
    Text render BlackFontID CentreAlign TopAlign x y message

    #if SHORT_PLAYTHROUGH
    // As the score panel is usually drawn last, this should work:
    Text render RedFontID CentreAlign MiddleAlign 160<epx> 10<epx> "WARNING  SHORT PLAY VERSION"
    #endif
