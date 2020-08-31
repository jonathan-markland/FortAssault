module ScorePanel

open Angle
open ScoreHiScore
open SharedDrawing
open ImagesAndFonts
open DrawingCommandsEx
open FontAlignment
open Geometry

[<Struct>]
type ScorePanel =
    {
        ScoreAndHiScore:  ScoreAndHiScore
        ShipsPending:     uint32
        ShipsThrough:     uint32
        Tanks:            uint32
        Damage:           uint32
        Ammunition:       uint32
        Elevation:        float32<degrees>
    }

let ShipBarChart panel =
    {
        BlueCount     = panel.ShipsThrough
        RedCount      = panel.ShipsPending
        RedCharIndex  = 2u
        BlueCharIndex = 4u
        CharSpacing   = 8u  // TODO: constant
    }

let TanksBarChart panel =
    {
        BlueCount     = panel.Tanks
        RedCount      = 0u
        RedCharIndex  = 1u
        BlueCharIndex = 3u
        CharSpacing   = 8u  // TODO: constant
    }

let AmmoBarChart panel =
    {
        BlueCount     = panel.Ammunition
        RedCount      = 0u
        RedCharIndex  = 0u
        BlueCharIndex = 0u
        CharSpacing   = 2u * 8u  // TODO: constant 8
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
    Bar  render c2x shy (panel |> ShipBarChart)

    let dmy = y + 24<epx>
    Text render YellowFontID LeftAlign TopAlign c1x dmy "DAMAGE"
    Num  render BlueFontID LeftAlign TopAlign c2x dmy (panel.Damage)

    // TODO: The ammunition concept was limiting firing repeats, but it's not
    //       used, in favour of having to wait.  Need to introduce a use for
    //       this perhaps in future.
    //
    // Text render YellowFontVisual LeftAlign TopAlign c3x y "AMMUNITION"
    // Bar  render c3x (y + 8<wu>) (panel |> AmmoBarChart)

    let tay = y + 16<epx>
    Text render YellowFontID LeftAlign TopAlign c3x tay "TANKS"
    Bar  render (c3x + 48<epx>) tay (panel |> TanksBarChart)

    let ely = y + 24<epx>
    Text render YellowFontID LeftAlign TopAlign c3x ely "ELEVATION"
    Flo  render BlueFontID LeftAlign TopAlign (c3x + 96<epx>) ely (float32 panel.Elevation)


let DrawTankBattleScorePanel render y (score:uint32) (numTanks:uint32) =

    let x = ScreenWidthInt / 2
    let y = y + 2<epx>

    let message = sprintf "SCORE  %d     TANKS  %d" score numTanks
    Text render BlackFontID CentreAlign TopAlign x y message
