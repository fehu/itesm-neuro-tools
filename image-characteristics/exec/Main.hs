-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- | See @wick -h@.
--

{-# LANGUAGE GADTs, FlexibleInstances #-}

module Main where

import Options
import MainChVec
import MainTrain
import MainClass

import CArgs


import ImgCharacteristics
import ImgCharacteristics.Friday

import Vision.Image (RGB)




-- Regions Extractor
-----------------------------------------------------------------------------


fixedRegions = FixedColRowRegions{ rRow = 50
                                 , rCol = 50
                                 , minRegionSize = (100, 100)
                                 }           -- (height, width)

instance RegionsExtractor RGB where foreachRegion  = fixedColRowRegions  fixedRegions
                                    foreachRegion' = fixedColRowRegions' fixedRegions

-- Main
-----------------------------------------------------------------------------

main = application arguments argsHandler{
      handleMain = \(m:.s:.t:.Nil) ->
                case posValue m of ARFF  -> mainChVec (posValue s) (posValue t)
                                   MODEL -> mainTrain (posValue s) (posValue t)
                                   CLASS -> mainClass (posValue s) (posValue t)
    , handleOpts = optsH
    }

optsH = handleHelp "wick" header arguments


----------------------------------------------------------------------------
----------------------------------------------------------------------------









