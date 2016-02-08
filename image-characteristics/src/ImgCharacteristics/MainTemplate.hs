-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.MainTemplate
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module ImgCharacteristics.MainTemplate where

import ImgCharacteristics
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday -- provides ImageToPixbuf RGB
import ImgCharacteristics.Weka

import System.Environment
import System.Exit
import System.Directory

import Data.Either
import Control.Monad

import Vision.Image (RGB)

-----------------------------------------------------------------------------


main' :: ( Num num
         , Show num
         , Class class'
         , RegionsExtractor RGB
         )
      =>
           ClassesInterview RGB class'
        -> CharacteristicsExtractor RGB num l
        -> IO ()
main' ci ce = getArgs >>= parseArgs' ci ce

parseArgs' :: ( Num num
              , Show num
              , Class class'
              , RegionsExtractor RGB
              )
          =>
              ClassesInterview RGB class'
           -> CharacteristicsExtractor RGB num l
           -> [String]
           -> IO ()

parseArgs' ci ce []     = showHelp >> exitFailure
parseArgs' ci ce ["-h"] = showHelp >> exitSuccess
parseArgs' ci ce [_]    = showHelp >> exitFailure
parseArgs' ci ce [_, _] = showHelp >> exitFailure
parseArgs' ci ce ["--dir", dir, relName, target] = undefined
parseArgs' ci ce args = do
    let target = last args
    let t' = init args
    let relName = last t'
    imgs' <- mapM readImage $ init t'

    let (failed, imgs) = partitionEithers imgs'

    cs <- collectCharacteristics ci ce imgs

    let !weka = learnDataToWeka (characteristicsNames ce) [minBound..maxBound] cs

    writeFile target $ stringifyWekaData relName weka

    putStrLn $ "wrote characteristics to file: " ++ target


collectCharacteristics ci ce imgs
    | null imgs = error "Nothing to do: no images provided"
    | otherwise = fmap concat . sequence $ map (extractLearnData ci ce) imgs




showHelp = putStrLn "TODO"
