-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}

module ImgCharacteristics (

  CharacteristicExtractor(..)
, CharacteristicsExtractor(..)
, addCharacteristic

, RegionsExtractor(..)
, ForeachRegion
, FixedColRowRegions(..)

, imageCharacteristics

) where

import Nat.Vec

-----------------------------------------------------------------------------


data (Num n) => CharacteristicExtractor img n = CharacteristicExtractor (img -> n) String

data (Num n) => CharacteristicsExtractor img n l = CharacteristicsExtractor {
                                                    characteristics      :: img -> Vec l n
                                                  , characteristicsNames :: Vec l String
                                                 }

addCharacteristic (CharacteristicsExtractor es ns) (CharacteristicExtractor e n) =
    CharacteristicsExtractor (\i -> e i +: es i) (n +: ns)


-----------------------------------------------------------------------------

type ForeachRegion img = forall a . img -> (img -> a) -> [a]

class RegionsExtractor img where foreachRegion :: ForeachRegion img

data FixedColRowRegions = FixedColRowRegions { rRow          :: Int
                                             , rCol          :: Int
                                             , minRegionSize :: (Int, Int)
                                             }

-----------------------------------------------------------------------------

imageCharacteristics :: (Num n, RegionsExtractor img) =>
                        CharacteristicsExtractor img n l
                     -> img
                     -> [Vec l n]
imageCharacteristics ce img = foreachRegion img
                            $ \i -> characteristics ce i


-----------------------------------------------------------------------------
