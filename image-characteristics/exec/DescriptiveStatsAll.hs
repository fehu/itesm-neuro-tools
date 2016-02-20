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
import Vision.Image.JuicyPixels
import Codec.Picture

import Data.Time
import Data.Time.Format

import System.FilePath
import System.Directory

data WildfireClass = Fire
                   | Smoke
                   | FireAndSmoke
                   | None
                   | Ignore
                   deriving (Enum, Bounded)

instance Show WildfireClass where show Fire = "Fire"
                                  show Smoke = "Smoke"
                                  show FireAndSmoke = "Fire&Smoke"
                                  show None = "Neither"
                                  show Ignore = "Ignore"

fixedRegions = FixedColRowRegions{ rRow = 50
                                 , rCol = 50
                                 , minRegionSize = (100, 100)
                                 }           -- (height, width)

instance RegionsExtractor RGB where foreachRegion = fixedColRowRegions fixedRegions

main = do ci <- classProvider :: IO (ClassesInterview RGB WildfireClass)

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

          createDirectoryIfMissing True reportsDir

          main' (Just save) ci e2





