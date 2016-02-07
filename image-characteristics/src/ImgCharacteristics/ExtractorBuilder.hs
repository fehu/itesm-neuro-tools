-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.ExtractorBuilder
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

module ImgCharacteristics.ExtractorBuilder (

  ExtractorBuilder
, SomeExtractorBuilder(..)
, ChanelExtractor(..)
, LinkedChanelExtractor(..)
, emptyBuilder
, someBuilder
, (+#)
, (+##)
, build

) where

import Nat.Vec
import ImgCharacteristics

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

data SomeExtractorBuilder n num = forall ns . (NatsOps ns) =>
    SomeExtractorBuilder (ExtractorBuilder n num ns) (Nat' (NatsSum ns))

someBuilder :: (NatsOps ns, NatsSum ns ~ r, NatRules r) =>
    ExtractorBuilder n num ns -> SomeExtractorBuilder n num
someBuilder b = SomeExtractorBuilder b undefined

emptyBuilder :: ExtractorBuilder n num NNil
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

