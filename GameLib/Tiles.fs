module Tiles

open Geometry

type TileMatrixTraits =
    {
        TilesHorizontally : int
        TilesVertically   : int
        TileWidthPixels   : int<epx>
        TileHeightPixels  : int<epx>
    }

type TileMatrixViewportWindow =
    {
        WindowLeft   : int<epx>
        WindowTop    : int<epx>
        WindowWidth  : int<epx>
        WindowHeight : int<epx>
    }

type TileMatrixOffset =
    {
        OffsetX : int<epx>
        OffsetY : int<epx>
    }

/// tilingOffset is relative to the top left of the viewportWindow.
/// f is called for all partially/fully visible tiles, and only ever
/// with indices that lie within the TileMatrixTraits extents.
let ForEachTileWithVisiblePortion tileMatrix viewportWindow tilingOffset f =

    let startPixelPositionAndTileIndex (side:int<epx>) offset =
        if offset <= 0<epx> then
            let tileIndex = (-offset) / side
            let pixelPos = offset + (side * tileIndex)
            struct (pixelPos,tileIndex)
        else
            struct (offset,0)

    let tileW = tileMatrix.TileWidthPixels
    let tileH = tileMatrix.TileHeightPixels

    let struct (drawX, indexX) = startPixelPositionAndTileIndex tileW tilingOffset.OffsetX
    let struct (drawY, indexY) = startPixelPositionAndTileIndex tileH tilingOffset.OffsetY

    // - drawX, drawY are relative to the top left of the viewportWindow.
    // - indexX, indexY will never be less than 0, but could be greater/equal to
    //   the tile-counts in respective directions of the matrix.

    let tileArrayCountX = tileMatrix.TilesHorizontally
    let tileArrayCountY = tileMatrix.TilesVertically  

    let viewLeft = viewportWindow.WindowLeft
    let viewTop  = viewportWindow.WindowTop

    if viewportWindow.WindowWidth > 0<epx> && viewportWindow.WindowHeight > 0<epx> then

        let endX = viewLeft + viewportWindow.WindowWidth
        let endY = viewTop  + viewportWindow.WindowHeight

        let mutable y = viewTop + drawY
        let mutable iy = indexY

        while y < endY && iy < tileArrayCountY do

            let mutable x = viewLeft + drawX
            let mutable ix = indexX

            while x < endX && ix < tileArrayCountX do
                f x y ix iy
                x <- x + tileW
                ix <- ix + 1

            y <- y + tileH
            iy <- iy + 1

        



