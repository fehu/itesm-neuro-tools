-----------------------------------------------------------------------------
--
-- Module      :  MainCommon
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TypeOperators #-}

module MainCommon where


-----------------------------------------------------------------------------

import Nat
import CArgs

import System.Directory
import System.FilePath

import ImgCharacteristics
import ImgCharacteristics.Friday

import Vision.Image (RGB)
import Vision.Image (convert, RGB, Convertible)


-----------------------------------------------------------------------------


listImages dir' = do let dir = text2str dir'
                     imgs' <- listDirectory dir
                     return $ map (dir </>) imgs'

-----------------------------------------------------------------------------

descrStatsAll :: CharacteristicsExtractor RGB Double (N8 :*: N7)
descrStatsAll = let eRGB  = extractorRGB  descriptiveStats
                    eHSV  = extractorHSV  descriptiveStats
                    eGrey = extractorGrey descriptiveStats

                    e1 :: CharacteristicsExtractor RGB Double (N8 :*: N3 :*: N2)
                    e1 = eRGB `combineExtractors` eHSV
                in e1 `combineExtractors` eGrey



-----------------------------------------------------------------------------
