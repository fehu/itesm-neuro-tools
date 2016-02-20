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

{-# LANGUAGE ExistentialQuantification
           , FlexibleInstances
           , GADTs
           , BangPatterns
           , ConstraintKinds
       #-}

module ImgCharacteristics (

  CharacteristicExtractor(..)
, CharacteristicsExtractor(..)
, addCharacteristic
, addCharacteristics
, characteristicsFromVec

, RegionsExtractor(..)
, ForeachRegion
, FixedColRowRegions(..)

, Class
, LearnDataEntry(..)
, RegionsClassesProvider(..)

, imageCharacteristics
, extractLearnData

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

type ForeachRegion   img = forall a . img -> (img -> (Int, Int) -> a) -> [a]

class RegionsExtractor img where foreachRegion   :: ForeachRegion img

data FixedColRowRegions = FixedColRowRegions { rRow          :: Int
                                             , rCol          :: Int
                                             , minRegionSize :: (Int, Int) -- ^ (height, width)
                                             }

-----------------------------------------------------------------------------

data LearnDataEntry l num class' = LearnDataEntry !(Vec l num, class')

instance (Show n, Show c) => Show (LearnDataEntry l n c) where
    show (LearnDataEntry (v,c)) = show c ++ ": " ++ show v

type Class c = (Show c, Enum c, Bounded c)

class RegionsClassesProvider p img where
    classProvider :: (Class class') => IO (p img class')
    regionClass   :: (Class class') => p img class' -> img -> IO class'

-----------------------------------------------------------------------------

imageCharacteristics :: (Num n, RegionsExtractor img) =>
                        CharacteristicsExtractor img n l
                     -> img
                     -> [Vec l n]
imageCharacteristics ce img = foreachRegion img
                            $ \i _ -> characteristics ce i

extractLearnData :: ( Num num
                    , RegionsExtractor img
                    , RegionsClassesProvider p img
                    , Class class'
                    ) =>
    p img class' -> CharacteristicsExtractor img num l
                 -> Maybe (img -> String -> (Int, Int) -> class' -> IO())
                 -> img
                 -> String -- ^ image name
                 -> IO [LearnDataEntry l num class']
extractLearnData p ce mbSave img iname = sequence $ foreachRegion img
                          $ \i ind -> do let !cs = strictVec $ characteristics ce i
                                         clz <- regionClass p i
                                         sequence_ $ do save <- mbSave
                                                        return $ save i iname ind clz
                                         return $ LearnDataEntry (cs, clz)


-----------------------------------------------------------------------------
