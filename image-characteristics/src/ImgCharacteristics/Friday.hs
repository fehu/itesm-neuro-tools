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

  -- for tests
, imgSizeCharacteristic


) where

import ImgCharacteristics
import ImgCharacteristics.Friday.Utils
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

-- | Extracts min, max, mean, stdev, quadratic mean and the three quartiles.
--descriptiveStats :: (Floating num, NatRules n, NatRules (n :+: n), GenVec n) => SomeExtractorBuilder n num
descriptiveStats :: (Floating num, NatRules n
                    , NatRules (N2 :*: n)
                    , NatRules (N3 :*: n)
--                    , NatRules ((N3 :*: n) :+: (N2 :*: n))
                    , GenVec n
                    ) =>
                SomeExtractorBuilder n num
descriptiveStats = someBuilder $ emptyBuilder +## CE.meanAndStdev
                                              +#  CE.quartiles

--descriptiveStats :: ( Floating num
--                    , Ord num
--                    , Image img
--                    , PixelDimsNat (ImagePixel img)  ~ n
--                    , PixelToVec (ImagePixel img)
--                    , GenVec n
--                    , NatRules n
----                    , (n :+: Zero) ~ n
--                    ) =>
--    Vec n String -> CharacteristicsExtractor img num (N8 :*: n)
--descriptiveStats chanelNames = let (minf, minn) = min' chanelNames
--                                   (maxf, maxn) = max' chanelNames
--                                   (mf, mnames) = mean' chanelNames
--                                   (sf, snames) = stdev' chanelNames
--                                   (mqf, mqn)   = meanQuadratic' chanelNames
--                                   (q4f, q4n)   = quartiles' chanelNames
--                                   cf img = let pixels = map (fmap fromIntegral)
--                                                       $ imgPixels img
--                                                m = mf pixels
--                                                s = sf m pixels
--                                                mn = minf pixels
--                                                mx = maxf pixels
--                                                mq = mqf pixels
--                                                q4 = q4f pixels
--                                          in mn +:+ mx +:+ m +:+ s +:+ mq +:+ q4
--                            in CharacteristicsExtractor cf
--                                $ minn +:+ maxn +:+ mnames +:+ snames +:+ mqn +:+ q4n


-----------------------------------------------------------------------------

-----------------------------------------------------------------------------


--t :: ExtractorBuilder N3 Double (NCons N9 (NCons N6 NNil))
t :: ExtractorBuilder N3 Double (NCons N9 (NCons N6 NNil))
t = emptyBuilder +## LinkedChanelExtractor CE.mean
                                          (CE.stdev' +: VNil)
                 +# CE.quartiles

extractorRGB :: ( PixelDimsNat (ImagePixel img) ~ N3
                , NatsSum ns ~ r
                , NatRules r
                , Fractional num
                , NatsOps ns
                , Image img
                , PixelToVec (ImagePixel img)
                ) =>
                ExtractorBuilder N3 num ns -> CharacteristicsExtractor img num r
extractorRGB b = build b (map (fmap fromIntegral) . imgPixels) $ genVecFrom nat3 $ map (:[]) "RGB"


