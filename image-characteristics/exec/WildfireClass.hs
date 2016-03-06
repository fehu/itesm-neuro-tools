-----------------------------------------------------------------------------
--
-- Module      :  WildfireClass
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module WildfireClass (

  WildfireClass(..)
, hasFireSigns

) where


import ImgCharacteristics

-----------------------------------------------------------------------------


data WildfireClass = Fire   -- ^ Region contains fire.
                   | Smoke  -- ^ Region contains smoke, but no sign fire.
                   | None   -- ^ Region contains neither.
                   | Ignore -- ^ Do not use the region for training.
                   | Unknown
                   deriving (Enum, Bounded, Eq)

instance Show WildfireClass where show Fire = "Fire"
                                  show Smoke = "Smoke"
                                  show None = "Neither"
                                  show Ignore = "Ignore"
                                  show Unknown = "?"

instance Class WildfireClass where classUnknown = Unknown


hasFireSigns x = x == Fire || x == Smoke

