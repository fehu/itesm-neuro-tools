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

, ExtractorBuilder
, ChanelExtractor(..)
, LinkedChanelExtractor(..)
, emptyBuilder
, (+#)
, (+##)
, extractorRGB

--, ImgCharacteristics.Friday.mean
, descriptiveStats

  -- for tests
, imgSizeCharacteristic
, t


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
import Data.Type.Equality

import Control.Arrow (first)

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

chanelCharacteristicsExtractor opName f chanelNames = (f, chNames opName chanelNames)

type ChanelCharacteristicsExtractor n num =
        (Floating num, GenVec n) =>
    Vec n String -> ([Vec n num] -> Vec n num, Vec n String)

type ChanelCharacteristicsExtractor' n num =
        (Floating num, GenVec n) =>
    Vec n String -> (Vec n num -> [Vec n num] -> Vec n num, Vec n String)

square a = a * a

foldVecs i f = vecsFoldr f (genVec (const i) undefined)

vecsSum :: (Num num, GenVec n) => [Vec n num] -> Vec n num
vecsSum = foldVecs 0 (+) -- foldr (vecCombine (+)) (genVec (const 0) undefined)

vecsNormalized f pixels = (/ len) <$> f pixels
    where len = fromIntegral $ length pixels

vecsNormalizedSum :: (Fractional num, GenVec n) => [Vec n num] -> Vec n num
vecsNormalizedSum = vecsNormalized (foldVecs 0 (+))

newtype Mean n num = Mean (Vec n num)
--newtype Variance n num = Variance (Vec n num)
--newtype StandardDeviation n num = StandardDeviation (Vec n num)
--
--newtype Min n num = Min (Vec n num)
--newtype Max n num = Max (Vec n num)
--
--newtype Quartile n num = Quartile (Vec n num)
--
--class AsVec a where asVec :: a n num -> Vec n num
--
--instance AsVec Mean where asVec (Mean x) = x
--instance AsVec Variance where asVec (Variance x) = x
--instance AsVec StandardDeviation where asVec (StandardDeviation x) = x
--instance AsVec Min where asVec (Min x) = x
--instance AsVec Max where asVec (Max x) = x
--instance AsVec Quartile where asVec (Quartile x) = x


mean' :: ChanelCharacteristicsExtractor n num
mean' = chanelCharacteristicsExtractor "mean" vecsNormalizedSum

meanGeom' :: ChanelCharacteristicsExtractor n num
meanGeom' = chanelCharacteristicsExtractor "geometric mean" f
    where f pixels = root <$> foldVecs 1 (*) pixels
            where root x = x ** (1/len)
                  len = fromIntegral $ length pixels

meanQuadratic' :: ChanelCharacteristicsExtractor n num
meanQuadratic' = chanelCharacteristicsExtractor "quadratic mean"
          $ fmap sqrt . vecsNormalized (foldVecs 0 f)
    where f x acc = acc + x*x

var' :: ChanelCharacteristicsExtractor' n num
var' names = (f, chNames "variance" names)
    where f mean pixels = vecsNormalizedSum mmsq
                where mmsq = map (fmap square . flip (vecCombine (-)) mean)
                                 pixels

stdev' :: ChanelCharacteristicsExtractor' n num
stdev' names = (f, chNames "stdev" names)
    where f mean pixels = fmap sqrt (fst (var' names) mean pixels)

binOpCollect' resName g = chanelCharacteristicsExtractor resName f
    where f (h:t) = vecsFoldr g h t


min' :: (Ord num) => ChanelCharacteristicsExtractor n num
min' = binOpCollect' "min" Prelude.min

max' :: (Ord num) => ChanelCharacteristicsExtractor n num
max' = binOpCollect' "max" Prelude.max

-----------------------------------------------------------------------------

-- TODO: odd/even cases
quantiles :: ( Succ qm ~ q
             , Fractional num
             , GenVec qm
             ) => Nat' q -> [Vec n num] -> Vec qm (Vec n num)
quantiles _ pixels = let len = length pixels
                         qsz = len `quot` 4
                         inds = genVec (* qsz) undefined
                   in fmap (pixels !!) inds

quartiles' :: ( Floating num
              , GenVec n
              ) =>
    Vec n String -> ([Vec n num] -> Vec (N3 :*: n) num, Vec (N3 :*: n) String)

quartiles' chanelNames = (vecsConcat . quantiles nat4, vecsConcat names)
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

mean chanelNames = CharacteristicsExtractor (f . map (fmap fromIntegral) . imgPixels) names
    where (f, names) = mean' chanelNames



-- | Extracts min, max, mean, stdev, quadratic mean and the three quartiles.
descriptiveStats :: ( Floating num
                    , Ord num
                    , Image img
                    , PixelDimsNat (ImagePixel img)  ~ n
                    , PixelToVec (ImagePixel img)
                    , GenVec n
                    , NatRules n
--                    , (n :+: Zero) ~ n
                    ) =>
    Vec n String -> CharacteristicsExtractor img num (N8 :*: n)
descriptiveStats chanelNames = let (minf, minn) = min' chanelNames
                                   (maxf, maxn) = max' chanelNames
                                   (mf, mnames) = mean' chanelNames
                                   (sf, snames) = stdev' chanelNames
                                   (mqf, mqn)   = meanQuadratic' chanelNames
                                   (q4f, q4n)   = quartiles' chanelNames
                                   cf img = let pixels = map (fmap fromIntegral)
                                                       $ imgPixels img
                                                m = mf pixels
                                                s = sf m pixels
                                                mn = minf pixels
                                                mx = maxf pixels
                                                mq = mqf pixels
                                                q4 = q4f pixels
                                          in mn +:+ mx +:+ m +:+ s +:+ mq +:+ q4
                            in CharacteristicsExtractor cf
                                $ minn +:+ maxn +:+ mnames +:+ snames +:+ mqn +:+ q4n


-----------------------------------------------------------------------------

newtype ChanelExtractor n num n2 =
    ChanelExtractor (Vec n String -> ([Vec n num] -> Vec n2 num, Vec n2 String))

data LinkedChanelExtractor n num l = LinkedChanelExtractor {
    lceMaster :: ChanelExtractor n num n
  , lceDepend :: Vec l (Vec n String -> (Vec n num -> [Vec n num] -> Vec n num, Vec n String))
  }

---- | might be needed in case of different dimensions of the dependencies results.
--data LinkedChanelExtractor1 n num n1 n2 =
--    LinkedChanelExtractor1  { lce1Master :: ChanelExtractor n num n1
--                            , lce1Depend :: Vec n1 num -> ChanelExtractor n num n2
--                            }
---- | might be needed in case of different dimensions of the dependencies results.
--data LinkedChanelExtractor2 n num n1 n2 n3 =
--    LinkedChanelExtractor2  { lce2Master  :: ChanelExtractor n num n1
--                            , lce2Depend1 :: Vec n1 num -> ChanelExtractor n num n2
--                            , lce2Depend2 :: Vec n2 num -> ChanelExtractor n num n3
--                            }


newtype ExtractorBuilder n num ns = ExtractorBuilder (Nats (ChanelExtractor n num) ns)

emptyBuilder = ExtractorBuilder NatsNil


(+#) :: ExtractorBuilder n num ns
     -> ChanelExtractor n num n2
     -> ExtractorBuilder n num (NCons n2 ns)
(+#) (ExtractorBuilder nats) ce = ExtractorBuilder $ ce +:: nats

(+##) :: ExtractorBuilder n num ns
      -> LinkedChanelExtractor n num l
      -> ExtractorBuilder n num (NCons (Succ l :*: n) ns)
(+##) (ExtractorBuilder nats) (LinkedChanelExtractor (ChanelExtractor m) ds) =
    ExtractorBuilder $ ChanelExtractor chd +:: nats
    where chd chnames = let (f0, n0) = m chnames
                            f pixels = let v0 = f0 pixels
                                     in v0 +: vecAccMap (g pixels) v0 ds
                            g pixels a e = let (ef,_) = e chnames
                                             in ef a pixels
                            nRest = fmap (snd . ($ chnames)) ds
                            in (vecsConcat . f, vecsConcat $ n0 +: nRest)


newtype GetCharacteristics    n num (n2 :: Nat) = GetCharacteristics ([Vec n num] -> Vec n2 num)
newtype CharacteristicsNames (n2 :: Nat)        = CharacteristicsNames (Vec n2 String)

prepareNPair :: ([Vec n num] -> Vec n2 num, Vec n2 String) -> NPair (GetCharacteristics n num) CharacteristicsNames n2
prepareNPair (f, n) = NPair (GetCharacteristics f, CharacteristicsNames n)

build :: ( (NatsSum ns) ~ r
         , NatRules r
         , NatsOps ns
         , Fractional num
         ) =>
     ExtractorBuilder n num ns -> (img -> [Vec n num]) -> Vec n String -> CharacteristicsExtractor img num r --(img -> Vec r num, Vec r String)
build (ExtractorBuilder nats) toPixels chanelNames = CharacteristicsExtractor f names
    where x = mapNats (\(ChanelExtractor che) -> prepareNPair $ che chanelNames) nats
          (fs', ns') = natsUnzip x
          f img = let pixels = toPixels img
                in natsFlatten (\(GetCharacteristics f') -> f' pixels) fs'
          names = natsFlatten (\(CharacteristicsNames ns) -> ns) ns'

-----------------------------------------------------------------------------


t :: ExtractorBuilder N3 Double (NCons N9 (NCons N6 NNil))
t = emptyBuilder +## LinkedChanelExtractor (ChanelExtractor mean')
                                           (stdev' +: VNil)
                 +# ChanelExtractor quartiles'

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


