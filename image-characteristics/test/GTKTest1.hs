
{-# LANGUAGE FlexibleInstances, TypeOperators, FlexibleContexts #-}

module Main where

import Nat.Vec

import ImgCharacteristics
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday

import System.Environment
import System.Exit
import Control.Monad

import Vision.Image (RGB)

main = getArgs >>= parseArgs

fixedRegions = FixedColRowRegions{ rRow = 5
                                 , rCol = 5
                                 , minRegionSize = (200, 200)
                                 }           -- (height, width)


data Class' = A | B | C deriving (Show, Enum, Bounded)

instance RegionsExtractor RGB where foreachRegion = fixedColRowRegions fixedRegions
                                    foreachRegionIO = fixedColRowRegions fixedRegions


parseArgs [imgPath] = do putStrLn $ "Processing " ++ imgPath ++ " ...\n"
                         Right img <- readImage imgPath

                         let ce :: CharacteristicsExtractor RGB Double (N8 :*: N3)
                             ce = extractorRGB descriptiveStats

                         p  <- classProvider :: IO (ClassesInterview RGB Class')
                         cs <- extractLearnData p ce img

                         putStrLn "Characteristics:"
                         forM_ (vec2list $ characteristicsNames ce) $ putStrLn . ("\t" ++ )
                         putStrLn "Characteristic Vectors:"
                         forM_ cs print



parseArgs args = putStrLn ("wrong arguments: " ++ show args) >> exitFailure

