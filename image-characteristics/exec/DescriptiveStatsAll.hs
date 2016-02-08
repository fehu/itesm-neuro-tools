-- DescriptiveStatsAll


{-# LANGUAGE FlexibleInstances, TypeOperators #-}

module Main where

import Nat.Vec

import ImgCharacteristics
import ImgCharacteristics.MainTemplate
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday

import Vision.Image (RGB, HSV, Grey)

fixedRegions = FixedColRowRegions{ rRow = 5
                                 , rCol = 5
                                 , minRegionSize = (200, 200)
                                 }           -- (height, width)

instance RegionsExtractor RGB where foreachRegion   = fixedColRowRegions fixedRegions
                                    foreachRegionIO = fixedColRowRegions fixedRegions

main = do ci <- classProvider :: IO (ClassesInterview RGB Bool)

          let eRGB  = extractorRGB  descriptiveStats
              eHSV  = extractorHSV  descriptiveStats
              eGrey = extractorGrey descriptiveStats

              e1 :: CharacteristicsExtractor RGB Double (N8 :*: N3 :*: N2)
              e1 = eRGB `combineExtractors` eHSV
              e2 = e1 `combineExtractors` eGrey

          main' ci e2





