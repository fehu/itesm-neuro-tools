-----------------------------------------------------------------------------
--
-- Module      :  Main (ImgCharacteristicsTest1)
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Nat.Vec

import ImgCharacteristics
import ImgCharacteristics.Friday

import Vision.Image hiding (map)
import Vision.Image.JuicyPixels (toFridayRGB)
import Vision.Image.RGB.Type
import Codec.Picture

import System.Environment

-----------------------------------------------------------------------------

main = getArgs >>= parseArgs

fixedRegions = FixedColRowRegions 5 5 (200, 200)

instance RegionsExtractor RGB where foreachRegion = fixedColRowRegions fixedRegions

parseArgs [imgPath] = do putStrLn $ "Processing " ++ imgPath
                         Right img' <- readImage imgPath
                         let img = toFridayRGB $ convertRGB8 img'

                         let ce = imgSizeCharacteristic
                         let cs = imageCharacteristics ce img

                         putStrLn "Characteristics Vector:"
                         putStrLn . show $ map (map show . vec2list) cs

