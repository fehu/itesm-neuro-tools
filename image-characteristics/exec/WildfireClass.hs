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
, mighHaveFireSigns

) where


import ImgCharacteristics

-----------------------------------------------------------------------------

data WildfireClass = Fire
                   | FireAndSmoke
                   | BrightSmoke
                   | Smoke
                   | SmokeOrSky
                   | None
                   | Ignore
                   | Unknown
                   deriving (Enum, Bounded, Eq)

instance Show WildfireClass where show Fire = "Fire"
                                  show FireAndSmoke = "Fire & Smoke"
                                  show BrightSmoke = "Bright Smoke"
                                  show Smoke = "Smoke"
                                  show SmokeOrSky = "Smoke or Sky"
                                  show None = "No Sign of Fire"
                                  show Ignore = "Ignore"
                                  show Unknown = "?"

instance Class WildfireClass where classUnknown = Unknown


hasFireSigns x = x == Fire || x == Smoke || x == FireAndSmoke || x == BrightSmoke

mighHaveFireSigns x = x == SmokeOrSky
