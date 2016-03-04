-----------------------------------------------------------------------------
--
-- Module      :  MainChVec
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module MainChVec where


--import Nat
import CArgs

import WildfireClass
import Options
import MainCommon

import ImgCharacteristics
import ImgCharacteristics.GTK
--import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday
import qualified ImgCharacteristics.MainTemplate as MainT


import System.Directory
import System.FilePath

import Data.Maybe
import Data.Time

import Vision.Image (RGB)
import Codec.Picture
import Vision.Image.JuicyPixels



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Build characteristic vectors and write to file.
mainChVec :: (RegionsExtractor RGB) =>
             Text           -- ^ Source images directory.
          -> Text           -- ^ Target *.arff file.
          -> OptionalValues
          -> Verbosity
          -> IO ()
mainChVec dir' arff opts verb = do
    imgs <- listImages dir'
    -- Optional args
    let
        withoutClass = isJust $ opts `get` noClass
        reportsRoot = fmap text2str (opts `get` saveRegions)
        relName = maybe "wildfire" text2str (opts `get` relationName)

        target = text2str arff

    -- Save reports
    time <- getCurrentTime
    let reportsDir root = root ++ pathSeparator:
                          formatTime defaultTimeLocale "%F_%T" time

        save :: String -> RGB -> String -> (Int,Int) -> WildfireClass -> IO()
        save root img nme (row,col) c =
          let (name, ext) = splitExtension nme
              filename = name ++ "_" ++ show row ++ "-" ++ show col
                      ++ "_" ++ show c ++ ext
          in writePng (reportsDir root ++ "/" ++ filename) $ toJuicyRGB img
        save' = fmap save reportsRoot
    -- Create reports dir
    mapM_ (createDirectoryIfMissing True . reportsDir) reportsRoot


    -- Runner
    let run :: (RegionsClassesProvider p RGB) => p RGB WildfireClass -> IO()
        run ci = MainT.collectImagesCharacteristics ci descrStatsAll relName imgs target save'

    -- Run
    if withoutClass
     then run =<< (classProvider False :: IO (RegionsClassUnknown RGB WildfireClass))
     else run =<< (classProvider False :: IO (ClassesInterview    RGB WildfireClass))


-----------------------------------------------------------------------------

