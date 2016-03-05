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
import ImgCharacteristics.GTK.ImageTiles
import ImgCharacteristics.GTK.FromFriday

import Vision.Image (RGB)
import Graphics.UI.Gtk

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
    imgs     <- readImages imgPaths

    wTiles <- imageTilesWindow


    withWekaHomeEnv extraClasspath $ do
        ch <- loadModel $ text2str modelF

        classification <- forM imgs
            $ \(img,file) -> do
                let (rCount, rSize, foreachR) = foreachRegion' img
                iTiles <- io $ newImgTiles wTiles rCount rSize
                a <- assemble file =<< sequence (foreachR (processRegion iTiles ch))
                io $ waitClick iTiles
                return a

        let (ok, warn) = partition (null . snd) classification

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
                 ImageTiles RGB
              -> (c, Instances')
              -> RGB
              -> (Int,Int)
              -> Java (WildfireClass, (Int, Int))
processRegion iTiles (c, header) ri rxy = do
    let chs = characteristics descrStatsAll ri
    inst <- makeInstance header chs
    clazz <- classifyNom c inst
    io $ setRegion iTiles rxy ri (classColor clazz)
    return (clazz, rxy)


classColor c = case c of Fire         -> Color 255 0   0
                         Smoke        -> Color 128 128 128
                         FireAndSmoke -> Color 204 102 0
                         None         -> Color 0   255 0
                         _            -> Color 255 255 255

-----------------------------------------------------------------------------
