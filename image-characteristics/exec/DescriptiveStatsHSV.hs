-- DescriptiveStatsHSV


{-# LANGUAGE FlexibleInstances #-}

module Main where

import ImgCharacteristics
import ImgCharacteristics.MainTemplate
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday

import Vision.Image (HSV)

fixedRegions = FixedColRowRegions{ rRow = 5
                                 , rCol = 5
                                 , minRegionSize = (200, 200)
                                 }           -- (height, width)

instance RegionsExtractor HSV where foreachRegion = fixedColRowRegions fixedRegions

main = do ci <- classProvider :: IO (ClassesInterview HSV Bool)
          main' Nothing ci (extractorHSV descriptiveStats)





