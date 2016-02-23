-- DescriptiveStatsAll_NoClass


{-# LANGUAGE FlexibleInstances, TypeOperators, MultiParamTypeClasses #-}

module Main where

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

data UnknownClass = UnknownClass deriving (Enum, Bounded)

instance Show UnknownClass where show _ = "?"

fixedRegions = FixedColRowRegions{ rRow = 50
                                 , rCol = 50
                                 , minRegionSize = (100, 100)
                                 }           -- (height, width)

instance RegionsExtractor RGB where foreachRegion   = fixedColRowRegions fixedRegions
                                    foreachRegionIO = fixedColRowRegions fixedRegions

data ClassesInterviewStub img class'

instance RegionsClassesProvider ClassesInterviewStub img where
    classProvider = return undefined
    regionClass _ _ = return minBound


main = do ci <- classProvider :: IO (ClassesInterviewStub RGB UnknownClass)

          let eRGB  = extractorRGB  descriptiveStats
              eHSV  = extractorHSV  descriptiveStats
              eGrey = extractorGrey descriptiveStats

              e1 :: CharacteristicsExtractor RGB Double (N8 :*: N3 :*: N2)
              e1 = eRGB `combineExtractors` eHSV
              e2 = e1 `combineExtractors` eGrey

          main' Nothing ci e2





