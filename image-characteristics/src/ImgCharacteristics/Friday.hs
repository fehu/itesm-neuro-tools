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

{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeOperators #-}

module ImgCharacteristics.Friday (

  Img
, fixedColRowRegions

-- * Characteristics

-- TODO: zero histograms for some images ??
--, histogram
--, histogram3

, ImgCharacteristics.Friday.mean
, descriptiveStats

  -- for tests
, imgSizeCharacteristic

) where

import ImgCharacteristics
import ImgCharacteristics.Friday.Utils

import Nat.Vec

import Vision.Image hiding (map)
import Vision.Primitive
import Vision.Primitive.Shape ( (:.)(..), Z(..) )
import Vision.Image.Transform ( crop )
import qualified Vision.Histogram as H
import qualified Data.Vector.Storable as V

import Foreign.Storable (Storable)
import Data.Int
import System.IO.Unsafe

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

chNames opName = fmap ((opName ++ " ") ++)

chanelCharacteristicsExtractor' mkRes opName f chanelNames = (mkRes . f, chNames opName chanelNames)

type ChanelCharacteristicsExtractor' dep res n num1 num2 =
        (Integral num1, Floating num2, GenVec n) =>
    Vec n String -> (dep -> [Vec n num1] -> res n num2, Vec n String)

square a = a * a

vecsSum :: (Num num, GenVec n) => [Vec n num] -> Vec n num
vecsSum = foldr (vecCombine (+)) (genVec (const 0) undefined)

vecsNormalizedSum pixels = (/ len) <$> vecsSum pixels
    where len = fromIntegral $ length pixels

newtype Mean n num = Mean (Vec n num)
newtype Variance n num = Variance (Vec n num)
newtype StandardDeviation n num = StandardDeviation (Vec n num)

class AsVec a where asVec :: a n num -> Vec n num

instance AsVec Mean where asVec (Mean x) = x
instance AsVec Variance where asVec (Variance x) = x
instance AsVec StandardDeviation where asVec (StandardDeviation x) = x

mean' :: (Integral num1, Floating num2, GenVec n) =>
    Vec n String -> ([Vec n num1] -> Mean n num2, Vec n String)
mean' = chanelCharacteristicsExtractor' Mean "mean"
      $ vecsNormalizedSum . map (fmap fromIntegral)


var' :: ChanelCharacteristicsExtractor' (Mean n num2) Variance n num1 num2
var' names = (f, chNames "variance" names)
    where f (Mean mean) pixels = Variance $ vecsNormalizedSum mmsq
                where mmsq = map ( fmap square
                                 . flip (vecCombine (-)) mean
                                 . fmap fromIntegral
                                 )
                                 pixels


stdev' :: ChanelCharacteristicsExtractor' (Variance n num2) StandardDeviation n num1 num2
stdev' names = (f, chNames "stdev" names)
    where f (Variance var) _ = StandardDeviation $ fmap sqrt var


-----------------------------------------------------------------------------


imgPixels img = map pix2vec . V.toList $ vector img

mean :: ( Floating num
        , Image img
        , PixelDimsNat (ImagePixel img)  ~ n
        , PixelToVec (ImagePixel img)
        , GenVec n
        ) =>
    Vec n String -> CharacteristicsExtractor img num n

mean chanelNames = CharacteristicsExtractor (asVec . f . imgPixels) names
    where (f, names) = mean' chanelNames



-- | Extracts mean and stdev.
descriptiveStats :: ( Floating num
                    , Image img
                    , PixelDimsNat (ImagePixel img)  ~ n
                    , PixelToVec (ImagePixel img)
                    , GenVec n
                    ) =>
    Vec n String -> CharacteristicsExtractor img num (n :+: n)
descriptiveStats chanelNames = let (mf, mnames) = mean' chanelNames
                                   (vf, _)      = var' chanelNames
                                   (sf, snames) = stdev' chanelNames
                                   cf img = let pixels = imgPixels img
                                                m = mf pixels
                                                v = vf m pixels
                                                s = sf v pixels
                                          in asVec m +:+ asVec s
                            in CharacteristicsExtractor cf (mnames +:+ snames)


-----------------------------------------------------------------------------


