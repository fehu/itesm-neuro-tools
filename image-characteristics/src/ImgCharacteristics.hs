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
, RegionsCount, RegionsSize
, ForeachRegion, ForeachRegion'
, FixedColRowRegions(..)

, Class(..)
, LearnDataEntry(..)
, RegionsClassesProvider(..)
, RegionsClassUnknown

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

type RegionsCount = (Int, Int)
type RegionsSize  = (Int, Int)

type ForeachRegionFunc a b = a -> (Int, Int) -> b

type ForeachRegion img = forall a . img -> ForeachRegionFunc img a -> [a]

-- | Given an image, returns 1) number of regions; 2) region's size;
--    3) /foreachRegion/ function, that is 'ForeachRegion' with first argument applied.
type ForeachRegion' img = forall a . img -> ( RegionsCount
                                            , RegionsSize
                                            , ForeachRegionFunc img a -> [a]
                                            )




class RegionsExtractor img where foreachRegion  :: ForeachRegion  img
                                 foreachRegion' :: ForeachRegion' img


data FixedColRowRegions = FixedColRowRegions { rRow          :: Int
                                             , rCol          :: Int
                                             , minRegionSize :: (Int, Int) -- ^ (height, width)
                                             }

-----------------------------------------------------------------------------

data LearnDataEntry l num class' = LearnDataEntry !(Vec l num, class')

instance (Show n, Show c) => Show (LearnDataEntry l n c) where
    show (LearnDataEntry (v,c)) = show c ++ ": " ++ show v



class (Show c, Enum c, Bounded c, Eq c) => Class c where classUnknown :: c
                                                         classDomain  :: [c]

                                                         classDomain = filter (classUnknown /=)
                                                                       [minBound..maxBound]

class RegionsClassesProvider p img where
    classProvider :: (Class class') => Bool -- ^ show classUnknown?
                                    -> IO (p img class')
    regionClass   :: (Class class') => p img class' -> img -> IO class'


data RegionsClassUnknown img class'
instance RegionsClassesProvider RegionsClassUnknown img where
    classProvider _ = return undefined
    regionClass _ _ = return classUnknown

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
