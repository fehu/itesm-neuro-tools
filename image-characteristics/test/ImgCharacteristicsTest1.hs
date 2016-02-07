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

--{-# OPTIONS_GHC -fcontext-stack=400 #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators #-}

module Main where

import Nat.Vec

import ImgCharacteristics
import ImgCharacteristics.Friday as Fr

import Vision.Image hiding (map)
import Vision.Image.JuicyPixels (toFridayRGB)
import Vision.Image.RGB.Type
import Codec.Picture

import System.Environment
import System.Exit
import Control.Monad (forM_)

-----------------------------------------------------------------------------

main = getArgs >>= parseArgs

fixedRegions = FixedColRowRegions{ rRow = 5
                                 , rCol = 5
                                 , minRegionSize = (200, 200)
                                 }           -- (height, width)


instance RegionsExtractor RGB where foreachRegion = fixedColRowRegions fixedRegions

parseArgs [imgPath] = do putStrLn $ "Processing " ++ imgPath ++ " ...\n"
                         Right img' <- readImage imgPath
                         let img = toFridayRGB $ convertRGB8 img'

--                         let ce = imgSizeCharacteristic
--                         let hist :: CharacteristicsExtractor RGB Double (N4 :^: N3)
--                             hist = histogram3 "RGB 3D histogram:" nat4

--                         let rgbNames = genVecFrom nat3 $ map (:[]) "RGB"
--                         let stats :: CharacteristicsExtractor RGB Double (N8 :*: N3)
--                             stats = Fr.descriptiveStats rgbNames
--                         let ce = stats
--                             cs = imageCharacteristics ce img

                         let ce = extractorRGB t
                         let cs = imageCharacteristics ce img

                         putStrLn "Characteristics:"
                         forM_ (vec2list $ characteristicsNames ce) $ putStrLn . ("\t" ++ )
                         putStrLn "Characteristic Vectors:"
                         forM_ cs print



parseArgs args = putStrLn ("wrong arguments: " ++ show args) >> exitFailure
