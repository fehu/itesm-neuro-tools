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

{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeOperators, DataKinds #-}

module ImgCharacteristics.Friday (

  Img
, fixedColRowRegions

-- * Characteristics

-- TODO: zero histograms for some images ??
--, histogram
--, histogram3

--, ImgCharacteristics.Friday.mean
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
import Data.Type.Equality

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

chanelCharacteristicsExtractor mkRes opName f chanelNames = (mkRes . f, chNames opName chanelNames)

type ChanelCharacteristicsExtractor res n num =
        (Floating num, GenVec n) =>
    Vec n String -> ([Vec n num] -> res n num, Vec n String)

type ChanelCharacteristicsExtractor' dep res n num =
        (Floating num, GenVec n) =>
    Vec n String -> (dep -> [Vec n num] -> res n num, Vec n String)

square a = a * a

foldVecs i f = vecsFoldr f (genVec (const i) undefined)

vecsSum :: (Num num, GenVec n) => [Vec n num] -> Vec n num
vecsSum = foldVecs 0 (+) -- foldr (vecCombine (+)) (genVec (const 0) undefined)

vecsNormalized f pixels = (/ len) <$> f pixels
    where len = fromIntegral $ length pixels

vecsNormalizedSum :: (Fractional num, GenVec n) => [Vec n num] -> Vec n num
vecsNormalizedSum = vecsNormalized (foldVecs 0 (+))

newtype Mean n num = Mean (Vec n num)
newtype Variance n num = Variance (Vec n num)
newtype StandardDeviation n num = StandardDeviation (Vec n num)

newtype Min n num = Min (Vec n num)
newtype Max n num = Max (Vec n num)

newtype Quartile n num = Quartile (Vec n num)

class AsVec a where asVec :: a n num -> Vec n num

instance AsVec Mean where asVec (Mean x) = x
instance AsVec Variance where asVec (Variance x) = x
instance AsVec StandardDeviation where asVec (StandardDeviation x) = x
instance AsVec Min where asVec (Min x) = x
instance AsVec Max where asVec (Max x) = x
instance AsVec Quartile where asVec (Quartile x) = x


mean' :: ChanelCharacteristicsExtractor Mean n num
mean' = chanelCharacteristicsExtractor Mean "mean" vecsNormalizedSum

meanGeom' :: ChanelCharacteristicsExtractor Mean n num
meanGeom' = chanelCharacteristicsExtractor Mean "geometric mean" f
    where f pixels = root <$> foldVecs 1 (*) pixels
            where root x = x ** (1/len)
                  len = fromIntegral $ length pixels

meanQuadratic' :: ChanelCharacteristicsExtractor Mean n num
meanQuadratic' = chanelCharacteristicsExtractor Mean "quadratic mean"
          $ fmap sqrt . vecsNormalized (foldVecs 0 f)
    where f x acc = acc + x*x

var' :: ChanelCharacteristicsExtractor' (Mean n num) Variance n num
var' names = (f, chNames "variance" names)
    where f (Mean mean) pixels = Variance $ vecsNormalizedSum mmsq
                where mmsq = map (fmap square . flip (vecCombine (-)) mean)
                                 pixels


stdev' :: ChanelCharacteristicsExtractor' (Variance n num) StandardDeviation n num
stdev' names = (f, chNames "stdev" names)
    where f (Variance var) _ = StandardDeviation $ fmap sqrt var

binOpCollect' res resName g = chanelCharacteristicsExtractor res resName f
    where f (h:t) = vecsFoldr g h t


min' :: (Ord num) => ChanelCharacteristicsExtractor Min n num
min' = binOpCollect' Min "min" Prelude.min

max' :: (Ord num) => ChanelCharacteristicsExtractor Max n num
max' = binOpCollect' Max "max" Prelude.max

-----------------------------------------------------------------------------

-- TODO: odd/even cases
quantiles :: ( Succ qm ~ q
             , Fractional num
             , GenVec qm
             ) => Nat' q -> [Vec n num] -> Vec qm (Vec n num)
quantiles _ pixels = let len = length pixels
                         qsz = len `quot` 4
                         inds = genVec (* qsz) undefined
                   in fmap (pixels !!) inds -- . (-) 1

type Quartiles n num = Vec N3 (Vec n num)

quartiles' :: ( Floating num
              , GenVec n
              , (n :+: N0) ~ n
              ) =>
    Vec n String -> ([Vec n num] -> Quartiles n num, Vec N3 (Vec n String))

quartiles' chanelNames = (quantiles nat4, names)
    where names = genVec (\i -> chNames ("quartile " ++ show i) chanelNames) nat3


-----------------------------------------------------------------------------


imgPixels img = map pix2vec . V.toList $ vector img

mean :: ( Floating num
        , Image img
        , PixelDimsNat (ImagePixel img)  ~ n
        , PixelToVec (ImagePixel img)
        , GenVec n
        ) =>
    Vec n String -> CharacteristicsExtractor img num n

mean chanelNames = CharacteristicsExtractor (asVec . f . map (fmap fromIntegral) . imgPixels) names
    where (f, names) = mean' chanelNames



-- | Extracts min, max, mean, stdev, quadratic mean and the three quartiles.
descriptiveStats :: ( Floating num
                    , Ord num
                    , Image img
                    , PixelDimsNat (ImagePixel img)  ~ n
                    , PixelToVec (ImagePixel img)
                    , GenVec n
                    , (n :+: Zero) ~ n
                    ) =>
    Vec n String -> CharacteristicsExtractor img num (N8 :*: n)
descriptiveStats chanelNames = let (minf, minn) = min' chanelNames
                                   (maxf, maxn) = max' chanelNames
                                   (mf, mnames) = mean' chanelNames
                                   (vf, _)      = var' chanelNames
                                   (sf, snames) = stdev' chanelNames
                                   (mqf, mqn)   = meanQuadratic' chanelNames
                                   (q4f, q4n)   = quartiles' chanelNames
                                   cf img = let pixels = map (fmap fromIntegral)
                                                       $ imgPixels img
                                                m = mf pixels
                                                v = vf m pixels
                                                s = sf v pixels
                                                mn = minf pixels
                                                mx = maxf pixels
                                                mq = mqf pixels
                                                q4 = q4f pixels
                                          in asVec mn +:+ asVec mx +:+ asVec m
                                         +:+ asVec s  +:+ asVec mq +:+ vecsConcat q4
                            in CharacteristicsExtractor cf
                                $ minn   +:+ maxn +:+ mnames
                              +:+ snames +:+ mqn  +:+ vecsConcat q4n


-----------------------------------------------------------------------------


