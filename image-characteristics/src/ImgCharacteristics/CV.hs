-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.CV
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE BangPatterns #-}

module ImgCharacteristics.CV (

) where

import ImgCharacteristics

import CV.Image

--import System.IO.Unsafe


-----------------------------------------------------------------------------

fixedColRowRegions :: FixedColRowRegions -> ForeachRegion (Image c d) -- img -> (img -> a) -> [a]
fixedColRowRegions rd img f =
    do row <- [1..nRow]
       col <- [1..nCol]

       undefined
--       unsafePerformIO $ withIOROI ((col-1)*x, (row-1)*y)
--                                   (col*x, row*y)


    where ((x, y), nRow, nCol) = finalSize rd img
--          mimg = Mutable img

finalSize :: FixedColRowRegions -> Image c d -> ((Int, Int), Int, Int)
finalSize (FixedColRowRegions rRow rCol rMin) img
    | rRow == 0 ||
      rCol == 0     = ((0, 0), 0, 0)
    | (x, y) > rMin = ((x, y), rRow, rCol)
    | otherwise     = finalSize (FixedColRowRegions nRow nCol rMin) img

    where (width, height) = getSize img
          x = width `quot` rCol
          y = height `quot` rRow

          nRow = if x < fst rMin then rRow - 1 else rRow
          nCol = if x < snd rMin then rCol - 1 else rCol


-----------------------------------------------------------------------------
 -- -- -- -- -- -- -- -- -- -- Characteristics -- -- -- -- -- -- -- -- -- --
        -------------------------------------------------------------





