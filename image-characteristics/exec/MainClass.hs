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
import Options

import JUtils
import WekaCall
import WekaCall.Classify

import Weka.Core (Instances')

import Foreign.Java (forkJava)

import ImgCharacteristics
import ImgCharacteristics.MainTemplate
import ImgCharacteristics.GTK.ImageTiles
import ImgCharacteristics.GTK.FromFriday
import ImgCharacteristics.ParallelExec

import Vision.Image (RGB)
import Graphics.UI.Gtk

import Data.List (partition)
import Data.Maybe (isJust)
import Control.Monad

import Control.Concurrent.STM

-----------------------------------------------------------------------------

type Ctx = ((Classifier', Instances'), Maybe (TVar (Maybe (ImageTiles RGB))))

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

    let showGUI = isJust $ CArgs.get opts classificationGUI
        runPar  = CArgs.get opts classifyParallel

    mbWTiles <- if showGUI then do win <- imageTilesWindow
                                   var <- newTVarIO Nothing -- TODO: unused if not `runPar`
                                   return $ Just (win, var)
                           else return Nothing


    withWekaHomeEnv extraClasspath $ do
        ch <- loadModel $ text2str modelF

        -- Par definitions
        let init = return (ch, fmap snd mbWTiles)
            exec :: Ctx -> (RGB, (Int, Int)) -> Java (WildfireClass, (Int, Int))
            exec (ch, mbTilesVar) (img, xy) = do mbTiles <- mapM (fromIO . readTVarIO) mbTilesVar
                                                 processRegion (join mbTiles) ch img xy
        -- Par execution
        mbEx <- forM runPar $ \n -> parNew n (void . forkJava) init exec []


        classification <- forM imgs
            $ \(img,file) -> do
                let (rCount, rSize, foreachR) =
                        case mbEx of Just ex -> let (c,s,f) = foreachRegionPar' img
                                                in (c,s, const $ f ex)
                                     _       -> let (c,s,f) = foreachRegion' img
                                                in (c, s, \mbITiles' -> sequence
                                                                   $ f (processRegion mbITiles' ch)
                                                    )

                mbITiles <- forM mbWTiles $
                         \(wTiles, tilesVar) -> io $ do
                                iTiles <- newImgTiles wTiles rCount rSize :: IO (ImageTiles RGB)
                                when (isJust runPar) . atomically $ writeTVar tilesVar (Just iTiles)
                                return iTiles

                a <- assemble file =<< foreachR mbITiles
                forM_ mbITiles (io . waitClick)
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
                 Maybe (ImageTiles RGB)
              -> (c, Instances')
              -> RGB
              -> (Int,Int)
              -> Java (WildfireClass, (Int, Int))
processRegion mbITiles (c, header) ri rxy = do
    let chs = characteristics descrStatsAll ri
    inst <- makeInstance header chs
    clazz <- classifyNom c inst
    forM_ mbITiles (\iTiles -> io $ setRegion iTiles rxy ri (classColor clazz))
    return (clazz, rxy)

-----------------------------------------------------------------------------

classColor c = case c of Fire         -> Color 255 0   0    -- red
                         Smoke        -> Color 204 102 0    -- dark-orange
                         None         -> Color 0   255 0    -- green
                         _            -> Color 255 255 255  -- black

-----------------------------------------------------------------------------
