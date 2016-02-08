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


{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators #-}

module Main where

import Nat.Vec

import ImgCharacteristics
import ImgCharacteristics.Friday

import Vision.Image (RGB)

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
                                    foreachRegionIO = fixedColRowRegions fixedRegions

parseArgs [imgPath] = do putStrLn $ "Processing " ++ imgPath ++ " ...\n"
                         Right img <- readImage imgPath

                         let ce :: CharacteristicsExtractor RGB Double (N8 :*: N3)
                             ce = extractorRGB descriptiveStats

                             cs = imageCharacteristics ce img

                         putStrLn "Characteristics:"
                         forM_ (vec2list $ characteristicsNames ce) $ putStrLn . ("\t" ++ )
                         putStrLn "Characteristic Vectors:"
                         forM_ cs print



parseArgs args = putStrLn ("wrong arguments: " ++ show args) >> exitFailure
