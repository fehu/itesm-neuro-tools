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

  ChanelExtractor(..)
, LinkedChanelExtractor(..)
, linkedToExtractor

, (+#)
, (+##)

) where

import Nat.Vec
import ImgCharacteristics

-----------------------------------------------------------------------------


newtype ChanelExtractor n num c =
    ChanelExtractor (Vec n String -> ([Vec n num] -> Vec (c :*: n) num, Vec (c :*: n) String))

data LinkedChanelExtractor n num l = LinkedChanelExtractor {
    lceMaster :: ChanelExtractor n num N1
  , lceDepend :: Vec l (Vec n String -> (Vec n num -> [Vec n num] -> Vec n num, Vec n String))
  }

linkedToExtractor :: (NatRules n, ((c :+: N1) :*: n) ~ (n :+: (c :*: n))) =>
    LinkedChanelExtractor n num c -> ChanelExtractor n num (c :+: N1)
linkedToExtractor (LinkedChanelExtractor (ChanelExtractor m) ds) = ChanelExtractor chd
    where chd chnames = let (f0, n0) = m chnames
                            f pixels = let v0 = f0 pixels
                                     in v0 +: vecAccMap (g pixels) v0 ds
                            g pixels a e = let (ef,_) = e chnames
                                             in ef a pixels
                            nRest = fmap (snd . ($ chnames)) ds
                            in (vecsConcat . f, vecsConcat $ n0 +: nRest)


(+#) :: (NatRules3 c1 c2 n) =>
    ChanelExtractor n num c1 -> ChanelExtractor n num c2 -> ChanelExtractor n num (c1 :+: c2)
(+#) (ChanelExtractor ce1) (ChanelExtractor ce2) =
    ChanelExtractor $ \chnames -> let (f1, n1) = ce1 chnames
                                      (f2, n2) = ce2 chnames
                                      f img = f1 img +:+ f2 img
                                  in (f, n1 +:+ n2)

(+##) :: ( NatRules n, NatRules3 c1 c2 n
         , NatRules2 c2 n, NatRules3 c1 c2 N1
         , (((c1 :+: c2) :+: N1) :*: n)
            ~ ((c1 :*: n) :+: ((c2 :+: N1) :*: n))
         , (c1 :+: ((c2 :+: N1) :+: n))
            ~ (((c1 :+: c2) :+: N1) :+: n)
         ) =>
    ChanelExtractor n num c1 -> LinkedChanelExtractor n num c2 -> ChanelExtractor n num (c1 :+: c2 :+: N1)
(+##) ce lnk = ce +# linkedToExtractor lnk



