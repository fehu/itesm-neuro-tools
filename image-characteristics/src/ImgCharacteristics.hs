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

{-# LANGUAGE ExistentialQuantification #-}

module ImgCharacteristics (

  CharacteristicsExtractor(..)

, RegionsExtractor(..)
, ForeachRegion
, FixedColRowRegions(..)

, imageCharacteristics

) where

import Nat.Vec

-----------------------------------------------------------------------------

data (Num n) => CharacteristicsExtractor img n = CharacteristicsExtractor {
                                                    characteristic     :: img -> n
                                                  , characteristicName :: String
                                                }


-----------------------------------------------------------------------------

type ForeachRegion img = forall a . img -> (img -> a) -> [a]

class RegionsExtractor img where foreachRegion :: ForeachRegion img

data FixedColRowRegions = FixedColRowRegions { rRow          :: Int
                                             , rCol          :: Int
                                             , minRegionSize :: (Int, Int)
                                             }

-----------------------------------------------------------------------------

imageCharacteristics :: (Num n, RegionsExtractor img) =>
                        Vec l (CharacteristicsExtractor img n)
                     -> img
                     -> [Vec l n]
imageCharacteristics ces img = foreachRegion img
                             $ \i -> fmap (`characteristic` i) ces


-----------------------------------------------------------------------------
