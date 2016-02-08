-- DescriptiveStatsGray


{-# LANGUAGE FlexibleInstances #-}

module Main where

import ImgCharacteristics
import ImgCharacteristics.MainTemplate
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday

import Vision.Image (Grey)

fixedRegions = FixedColRowRegions{ rRow = 5
                                 , rCol = 5
                                 , minRegionSize = (200, 200)
                                 }           -- (height, width)

instance RegionsExtractor Grey where foreachRegion   = fixedColRowRegions fixedRegions
                                     foreachRegionIO = fixedColRowRegions fixedRegions

main = do ci <- classProvider :: IO (ClassesInterview Grey Bool)
          main' ci (extractorGrey descriptiveStats)





