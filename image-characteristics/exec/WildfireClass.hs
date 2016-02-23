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

) where


import ImgCharacteristics

-----------------------------------------------------------------------------

data WildfireClass = Fire
                   | Smoke
                   | FireAndSmoke
                   | None
                   | Ignore
                   | Unknown
                   deriving (Enum, Bounded, Eq)

instance Show WildfireClass where show Fire = "Fire"
                                  show Smoke = "Smoke"
                                  show FireAndSmoke = "Fire&Smoke"
                                  show None = "Neither"
                                  show Ignore = "Ignore"
                                  show Unknown = "?"

instance Class WildfireClass where classUnknown = Unknown
