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

  Img
, fixedColRowRegions

-- TODO: zero histograms for some images ??
--, histogram
--, histogram3

, descriptiveStats

, extractorRGB
, extractorHSV
, extractorGrey

, combineExtractors

  -- for tests
, imgSizeCharacteristic

, U.readImage

) where

import ImgCharacteristics
import ImgCharacteristics.Friday.Utils as U
import ImgCharacteristics.ExtractorBuilder
import ImgCharacteristics.Friday.Extractors as CE

import Nat.Vec

import Vision.Image hiding (map)
import Vision.Primitive
import Vision.Primitive.Shape ( (:.)(..), Z(..) )
import Vision.Image.Transform ( crop )
import qualified Vision.Histogram as H
import qualified Data.Vector.Storable as V

import Foreign.Storable (Storable)
import Data.Int


-----------------------------------------------------------------------------

type Img p = Manifest p
--

fixedColRowRegions :: (Storable p) => FixedColRowRegions -> ForeachRegion (Img p)
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
      rCol == 0      = ((0, 0), 0, 0)
    | (x, y) >= rMin = ((x, y), rRow, rCol)
    | otherwise      = finalSize (FixedColRowRegions nRow nCol rMin) img

    where Z :. height :. width = manifestSize img
          x = width `quot` rCol
          y = height `quot` rRow

          nRow = if y < fst rMin then rRow - 1 else rRow
          nCol = if x < snd rMin then rCol - 1 else rCol


-----------------------------------------------------------------------------
 -- -- -- -- -- -- -- -- -- -- Characteristics -- -- -- -- -- -- -- -- -- --
        -------------------------------------------------------------

-- for tests
imgSizeCharacteristic :: (Image img) => CharacteristicsExtractor img Int N2
imgSizeCharacteristic = CharacteristicsExtractor f names
    where f img = let Z :. h :. w = shape img
                in h +: w +: VNil
          names = "image height" +: "image width" +: VNil


-- TODO
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

histogram' h ix hName n = CharacteristicsExtractor f names
            where f img = let hist  = h (Just hSize) img
                              histN = H.normalize 100 hist
                        in genVecFrom n . map snd $ H.assocs histN
                  names = genVec(((hName ++ " ") ++ ) . show) n
                  hSize = ix (nat2int n)

-- | A normalized histogram for a single-channel image.
histogram :: ( Fractional num
             , Storable num
             , Nat2Integral n
             , GenVec n
             , Image img
             , H.PixelValueSpace (ImagePixel img) ~ DIM1
             , H.ToHistogram (ImagePixel img))
          =>
             String -> Nat' n -> CharacteristicsExtractor img num n

histogram = let h s i = H.histogram s i :: H.Histogram DIM1 Int32
          in histogram' h ix1

-- | A normalized histogram for a 3-channel image.
histogram3 :: ( Fractional num
             , Storable num
             , Nat2Integral n3
             , GenVec n3
             , Image img
             , H.PixelValueSpace (ImagePixel img) ~ DIM3
             , H.ToHistogram (ImagePixel img))
          =>
            ((n :^: N3) ~ n3) => String -> Nat' n -> CharacteristicsExtractor img num n3
histogram3 name _ = let h s i = H.histogram s i :: H.Histogram DIM3 Int32
                 in histogram' h (\i -> ix3 i i i) name (undefined :: Nat' n3)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


imgPixels img = map pix2vec . V.toList $ vector img

-- | Extracts min, the three quartiles, max, quadratic mean, mean and stdev.
descriptiveStats :: ( NatRules n, Floating num, Ord num, GenVec n
                    , NatRules3Pack n
                    ) => ChanelExtractor n num N8
descriptiveStats =    CE.min
                  +#  CE.quartiles
                  +#  CE.max
                  +#  CE.meanQuadratic
                  +## CE.meanAndVar

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

extractorRGB :: ( Fractional num) =>
            ChanelExtractor N3 num c -> CharacteristicsExtractor RGB num (c :*: N3)

extractorRGB (ChanelExtractor che) = CharacteristicsExtractor f names
    where (f' , names) = che $ genVecFrom nat3 $ map (:[]) "RGB"
          f = f' . map (fmap fromIntegral) . imgPixels


extractorHSV :: ( Fractional num) =>
            ChanelExtractor N3 num c -> CharacteristicsExtractor HSV num (c :*: N3)
extractorHSV (ChanelExtractor che) = CharacteristicsExtractor f names
    where (f' , names) = che $ genVecFrom nat3 $ map (:[]) "HSV"
          f = f' . map (fmap fromIntegral) . imgPixels


extractorGrey :: (Fractional num, NatRules c) =>
            ChanelExtractor N1 num c -> CharacteristicsExtractor Grey num c
extractorGrey (ChanelExtractor che) = CharacteristicsExtractor f names
    where (f' , names) = che $ genVecFrom nat1 $ map (:[]) "I"
          f = f' . map (fmap fromIntegral) . imgPixels


-----------------------------------------------------------------------------

combineExtractors :: ( Fractional num
                     , Convertible img0 img1
                     , Convertible img0 img2
                     ) =>
                     CharacteristicsExtractor img1 num c1
                  -> CharacteristicsExtractor img2 num c2
                  -> CharacteristicsExtractor img0 num (c1 :+: c2)
combineExtractors (CharacteristicsExtractor f1 ns1)
                  (CharacteristicsExtractor f2 ns2) =
    CharacteristicsExtractor f (ns1 +:+ ns2)
    where f img = f1 (convert img) +:+ f2 (convert img)



