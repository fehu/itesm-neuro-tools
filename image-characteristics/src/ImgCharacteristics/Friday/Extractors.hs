-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.Friday.Extractors
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

module ImgCharacteristics.Friday.Extractors (

  ImgCharacteristics.Friday.Extractors.min
, ImgCharacteristics.Friday.Extractors.max

, mean
, var'
, stdev'
, meanAndVar
, meanAndStdev

, meanGeom
, meanQuadratic

, quantiles'
, quartiles

) where

import Nat.Vec
import ImgCharacteristics.ExtractorBuilder


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

-----------------------------------------------------------------------------
-- -- -- -- -- -- -- -- -- -- --  Extractors -- -- -- -- -- -- -- -- -- -- --
        -------------------------------------------------------------

mean :: (Fractional num, GenVec n) => ChanelExtractor n num n
mean = ChanelExtractor $ chanelCharacteristicsExtractor "mean" vecsNormalizedSum

meanGeom :: (Floating num, GenVec n) => ChanelExtractor n num n
meanGeom = ChanelExtractor $ chanelCharacteristicsExtractor "geometric mean" f
    where f pixels = root <$> foldVecs 1 (*) pixels
            where root x = x ** (1/len)
                  len = fromIntegral $ length pixels

meanQuadratic :: (Floating num, GenVec n) => ChanelExtractor n num n
meanQuadratic = ChanelExtractor $ chanelCharacteristicsExtractor "quadratic mean"
          $ fmap sqrt . vecsNormalized (foldVecs 0 f)
    where f x acc = acc + x*x

-----------------------------------------------------------------------------

var' :: ChanelCharacteristicsExtractor' n num
var' names = (f, chNames "variance" names)
    where f mean pixels = vecsNormalizedSum mmsq
                where mmsq = map (fmap square . flip (vecCombine (-)) mean)
                                 pixels

meanAndVar :: (Floating num, GenVec n) => LinkedChanelExtractor n num N1
meanAndVar = LinkedChanelExtractor ImgCharacteristics.Friday.Extractors.mean
                                   (var' +: VNil)

-----------------------------------------------------------------------------

stdev' :: ChanelCharacteristicsExtractor' n num
stdev' names = (f, chNames "stdev" names)
    where f mean pixels = fmap sqrt (fst (var' names) mean pixels)

meanAndStdev :: (Floating num, GenVec n) => LinkedChanelExtractor n num N1
meanAndStdev = LinkedChanelExtractor ImgCharacteristics.Friday.Extractors.mean
                                     (var' +: VNil)

-----------------------------------------------------------------------------

binOpCollect' resName g = chanelCharacteristicsExtractor resName f
    where f (h:t) = vecsFoldr g h t


min :: (Ord num, Floating num, GenVec n) => ChanelExtractor n num n
min = ChanelExtractor $ binOpCollect' "min" Prelude.min

max :: (Ord num, Floating num, GenVec n) => ChanelExtractor n num n
max = ChanelExtractor $ binOpCollect' "max" Prelude.max

-----------------------------------------------------------------------------

-- TODO: odd/even cases
quantiles' :: ( Succ qm ~ q
             , Fractional num
             , GenVec qm
             ) => Nat' q -> [Vec n num] -> Vec qm (Vec n num)
quantiles' _ pixels = let len = length pixels
                          qsz = len `quot` 4
                          inds = genVec (* qsz) undefined
                    in fmap (pixels !!) inds

quartiles' :: ( Floating num
              , GenVec n
              ) =>
    Vec n String -> ([Vec n num] -> Vec (N3 :*: n) num, Vec (N3 :*: n) String)

quartiles' chanelNames = (vecsConcat . quantiles' nat4, vecsConcat names)
    where names = genVec (\i -> chNames ("quartile " ++ show i) chanelNames) nat3


quartiles :: (Ord num, Floating num, GenVec n) => ChanelExtractor n num (N3 :*: n)
quartiles = ChanelExtractor quartiles'

