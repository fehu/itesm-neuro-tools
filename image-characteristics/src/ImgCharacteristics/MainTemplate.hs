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
import ImgCharacteristics.WekaData

import System.Environment
import System.Exit
import System.Directory
import System.FilePath

import Data.Either
import Data.List (intercalate)
import Control.Monad
import Control.Arrow

import Vision.Image (convert, RGB, Convertible)

-----------------------------------------------------------------------------


main' :: ( Num num
         , Show num
         , Class class'
         , RegionsExtractor img
         , ImageToPixbuf img
         , Convertible RGB img
         , RegionsClassesProvider rcp img
         )
      =>
           Maybe (img -> String -> (Int,Int) -> class' -> IO())
        -> rcp img class'
        -> CharacteristicsExtractor img num l
        -> IO ()
main' mbSave ci ce = getArgs >>= parseArgs' mbSave ci ce


parseArgs' _ ci ce []     = showHelp >> exitFailure
parseArgs' _ ci ce ["-h"] = showHelp >> exitSuccess
parseArgs' _ ci ce [_]    = showHelp >> exitFailure
parseArgs' _ ci ce [_, _] = showHelp >> exitFailure

parseArgs' mbSave ci ce ["--dir", dir, relName, target] = do
    imgs' <- listDirectory dir
    let imgs = map (dir </>) imgs'
    collectImagesCharacteristics ci ce relName imgs target mbSave

parseArgs' mbSave ci ce args = do
    let target = last args
    let t' = init args
    let relName = last t'
    let imgs = init t'
    collectImagesCharacteristics ci ce relName imgs target mbSave


-----------------------------------------------------------------------------

-- from directory-1.2.5.0 due to broken dependencies
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."



collectImagesCharacteristics ci ce relName imgPaths target mbSave = do
    imgs' <- mapM readImage imgPaths

    let (failed, imgs) = partitionEithers imgs'

    forM_ failed putStrLn

    cs <- collectCharacteristics ci ce mbSave $ map (first convert) imgs

    let classDomain = filter (classUnknown /=) [minBound..maxBound]
        !weka = learnDataToWeka (characteristicsNames ce) classDomain cs

    writeFile target $ stringifyWekaData relName weka

    putStrLn $ "wrote characteristics to file: " ++ target



collectCharacteristics ci ce mbSave imgs
    | null imgs = error "Nothing to do: no images provided"
    | otherwise = fmap concat . sequence $ map (uncurry $ extractLearnData ci ce mbSave) imgs


-----------------------------------------------------------------------------

showHelp = putStrLn $ intercalate "\n" [
        "Usages:"
      , "  1. paths to images ... , relation name, target arff file"
      , "  2. --dir, path to images directory, relation name, target arff file"
      ]
