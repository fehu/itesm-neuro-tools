-----------------------------------------------------------------------------
--
-- Module      :  MainClass
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleContexts #-}

module MainClass where


import CArgs

import WildfireClass
import MainCommon

import JUtils
import WekaCall
import WekaCall.Classify

import Weka.Core (Instances')

import ImgCharacteristics
import ImgCharacteristics.MainTemplate

import Vision.Image (RGB)

import Data.List (partition)
import Control.Monad

-----------------------------------------------------------------------------


-- | Classify given images, based on its regions classification,
--   using the provided model.
mainClass :: (RegionsExtractor RGB) =>
             Text           -- ^ Model file.
          -> Text           -- ^ Directory, containing images to classify.
          -> OptionalValues
          -> Verbosity
          -> IO ()
mainClass modelF idir opts verb = do
    imgPaths <- listImages idir
    imgs <- readImages imgPaths

    withWekaHomeEnv extraClasspath $ do
        ch <- loadModel $ text2str modelF

        classification <- forM imgs
            $ \(img,file) -> assemble file =<< sequence (foreachRegion img (processRegion ch))

        let (ok, warn) = partition (null . snd) classification

--        unless (null ok) $ println "\n\tThe following images are OK:\n"
--        forM_ ok $ println . fst
--
--        unless (null warn) $ println "\n\tThe following images have sign of fire at given regions:\n"
--        forM_ warn $ \(f,rs) -> println $ f ++ '\t': unwords (map show rs)

        println $ "\n\tImages: " ++ show (length classification)
                 ++ "\tOK: "     ++ show (length ok)
                 ++ "\tDANGER: " ++ show (length warn)


resultStr file regions | null regions = "\nOK\t"     ++ file
                       | otherwise    = "\nDANGER\t" ++ file ++ '\t': unwords (map show regions)

-- | Any has Fire or//and Smoke?
assemble file classes = do let haveSigns = filter (hasFireSigns . fst) classes
                               regions   = map snd haveSigns
                           println $ resultStr file regions
                           return (file, regions)


-- | Get class for each region.
processRegion :: (RegionsExtractor RGB, Classifier c) =>
                 (c, Instances')
              -> RGB
              -> (Int,Int)
              -> Java (WildfireClass, (Int, Int))
processRegion (c, header) ri rxy = do
    let chs = characteristics descrStatsAll ri
    inst <- makeInstance header chs
    classifyNom c inst >>= (\x -> return (x, rxy))


-----------------------------------------------------------------------------
