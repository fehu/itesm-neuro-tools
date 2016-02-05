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

{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs #-}

module ImgCharacteristics (

  CharacteristicExtractor(..)
, CharacteristicsExtractor(..)
, addCharacteristic
, addCharacteristics
, characteristicsFromVec

, RegionsExtractor(..)
, ForeachRegion
, FixedColRowRegions(..)

, imageCharacteristics

) where

import Nat.Vec

-----------------------------------------------------------------------------


data (Num n) => CharacteristicExtractor img n = CharacteristicExtractor{
                                                    characteristic     :: img -> n
                                                  , characteristicName :: String
                                                  }

data (Num n) => CharacteristicsExtractor img n l = CharacteristicsExtractor {
                                                    characteristics      :: img -> Vec l n
                                                  , characteristicsNames :: Vec l String
                                                 }

addCharacteristic (CharacteristicsExtractor es ns) (CharacteristicExtractor e n) =
    CharacteristicsExtractor (\i -> e i +: es i) (n +: ns)

addCharacteristics (CharacteristicsExtractor es1 ns1) (CharacteristicsExtractor es2 ns2) =
    CharacteristicsExtractor (\i -> es1 i +:+ es2 i) (ns1 +:+ ns2)

characteristicsFromVec v = CharacteristicsExtractor cs names
    where cs img = fmap (($ img) . characteristic) v
          names = fmap characteristicName v

-----------------------------------------------------------------------------

type ForeachRegion img = forall a . img -> (img -> a) -> [a]

class RegionsExtractor img where foreachRegion :: ForeachRegion img

data FixedColRowRegions = FixedColRowRegions { rRow          :: Int
                                             , rCol          :: Int
                                             , minRegionSize :: (Int, Int) -- | (height, wifth)
                                             }

-----------------------------------------------------------------------------

imageCharacteristics :: (Num n, RegionsExtractor img) =>
                        CharacteristicsExtractor img n l
                     -> img
                     -> [Vec l n]
imageCharacteristics ce img = foreachRegion img
                            $ \i -> characteristics ce i


-----------------------------------------------------------------------------
