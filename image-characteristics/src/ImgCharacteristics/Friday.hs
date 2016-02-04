-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.Friday
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module ImgCharacteristics.Friday (

) where

import ImgCharacteristics

import Vision.Image
import Vision.Primitive
import Vision.Primitive.Shape ( (:.)(..), Z(..) )
import Vision.Image.Transform ( crop )
import qualified Vision.Histogram as H

import Foreign.Storable (Storable)

-----------------------------------------------------------------------------

type Img p = Manifest p
--

fixedColRowRegions :: (Storable (ImagePixel p), Storable p) =>
                      FixedColRowRegions
                   -> ForeachRegion (Img p)
fixedColRowRegions rd img f =
    do row <- [1..nRow]
       col <- [1..nCol]

       let region = Rect ((col-1)*x) ((row-1)*y) x y
       let img' = crop region img
       return $ f img'

    where ((x, y), nRow, nCol) = finalSize rd img

finalSize :: FixedColRowRegions -> Img p -> ((Int, Int), Int, Int)
finalSize (FixedColRowRegions rRow rCol rMin) img
    | rRow == 0 ||
      rCol == 0     = ((0, 0), 0, 0)
    | (x, y) > rMin = ((x, y), rRow, rCol)
    | otherwise     = finalSize (FixedColRowRegions nRow nCol rMin) img

    where Z :. height :. width = manifestSize img
          x = width `quot` rCol
          y = height `quot` rRow

          nRow = if x < fst rMin then rRow - 1 else rRow
          nCol = if x < snd rMin then rCol - 1 else rCol


-----------------------------------------------------------------------------
 -- -- -- -- -- -- -- -- -- -- Characteristics -- -- -- -- -- -- -- -- -- --
        -------------------------------------------------------------


--histogram img = H.histogram Nothing img








