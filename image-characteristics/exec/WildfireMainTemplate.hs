-----------------------------------------------------------------------------
--
-- Module      :  WildfireMainTemplate
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, TypeOperators, FlexibleContexts #-}

module WildfireMainTemplate (

  fixedRegions

, wildfireMain

) where

import Nat.Vec

import ImgCharacteristics
import ImgCharacteristics.MainTemplate
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday

import Vision.Image (RGB, HSV, Grey)
import Vision.Image.JuicyPixels
import Codec.Picture

import Data.Time
import Data.Time.Format

import System.FilePath
import System.Directory

import WildfireClass


fixedRegions = FixedColRowRegions{ rRow = 50
                                 , rCol = 50
                                 , minRegionSize = (100, 100)
                                 }           -- (height, width)

instance RegionsExtractor RGB where foreachRegion = fixedColRowRegions fixedRegions

wildfireMain saveRegions ci = do
          let eRGB  = extractorRGB  descriptiveStats
              eHSV  = extractorHSV  descriptiveStats
              eGrey = extractorGrey descriptiveStats

              e1 :: CharacteristicsExtractor RGB Double (N8 :*: N3 :*: N2)
              e1 = eRGB `combineExtractors` eHSV
              e2 = e1 `combineExtractors` eGrey

          time <- getCurrentTime
          let reportsDir = "reports/" ++ formatTime defaultTimeLocale "%F_%T" time

              save :: RGB -> String -> (Int,Int) -> WildfireClass -> IO()
              save img nme (row,col) c = let (name, ext) = splitExtension nme
                                             filename = name ++ "_" ++ show row ++ "-" ++ show col
                                                             ++ "_" ++ show c ++ ext
                                            in writePng (reportsDir ++ "/" ++ filename) $ toJuicyRGB img
              save' = if saveRegions then Just save else Nothing
          createDirectoryIfMissing True reportsDir

          main' save' ci e2
