-- DescriptiveStatsRGB


{-# LANGUAGE FlexibleInstances #-}

module Main where

import ImgCharacteristics
import ImgCharacteristics.MainTemplate
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday

import Vision.Image (RGB)

fixedRegions = FixedColRowRegions{ rRow = 5
                                 , rCol = 5
                                 , minRegionSize = (200, 200)
                                 }           -- (height, width)

instance RegionsExtractor RGB where foreachRegion   = fixedColRowRegions fixedRegions
                                    foreachRegionIO = fixedColRowRegions fixedRegions

main = do ci <- classProvider :: IO (ClassesInterview RGB Bool)
          main' ci (extractorRGB descriptiveStats)





