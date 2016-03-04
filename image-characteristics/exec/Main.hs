-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE GADTs
           , FlexibleContexts
           , TypeSynonymInstances
           , FlexibleInstances
           , TypeOperators
           #-}

module Main where

import Nat
import CArgs

import qualified GHC.Read as Read
import Text.Read (readMaybe)
import Data.Maybe
import Data.Time
import Data.Time.Format

import System.Directory
import System.FilePath

import ImgCharacteristics
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday
import ImgCharacteristics.GTK.FromFriday
import qualified ImgCharacteristics.MainTemplate as MainT

import Vision.Image (RGB)
import Codec.Picture
import Vision.Image.JuicyPixels

import WildfireClass
--import WildfireMainTemplate

-----------------------------------------------------------------------------

main = application arguments argsHandler{
      handleMain = \(m:.s:.t:.Nil) ->
                case posValue m of ARFF  -> mainChVec (posValue s) (posValue t)
                                   MODEL -> mainTrain (posValue s) (posValue t)
                                   CLASS -> mainClass (posValue s) (posValue t)
    , handleOpts = optsH
    }

-----------------------------------------------------------------------------

header = [ "\tWildfire Image Classification Kit (WICK)"
         , "\t-------------------------------  ----"
         , ""
         , "Conatains tools for 1) image characteristics extraction"
         , "                       (descriptive statistics);"
         , "                    2) Weka's Multilayer Perceptron model training;"
         , "                    3) image classification, using the model. "
         ]

data Mode = ARFF | MODEL | CLASS deriving Show

arguments = CArgs{
    positionalArguments =  Positional "mode"   mode modeDescr
                        :. Positional "source" text sourceDescr
                        :. Positional "target" text targetDescr
                        :. Nil
  , optionalArguments = [
        Opt helpArg
      , Opt verbArg
        -- ARFF
      , Opt saveRegions
      , Opt relationName
      , Opt noClass
        -- MODEL
      , Opt crossValidate
      , Opt saveReports
      , Opt nnetSettings
        -- CLASS

    ]
}


-----------------------------------------------------------------------------

instance Read Mode where readPrec = Read.parens
                                   $ Read.choose [
                                        ("arff",  return ARFF)
                                      , ("model", return MODEL)
                                      , ("class", return CLASS)
                                   ]

mode :: SingleParser Mode
mode = SingleParser "" readMaybe

-- Positionals
-----------------------------------------------------------------------------

modeDescr = [ "1. arff  -- extract characteristic vectors from images in given directory;"
            , "2. model -- train perceptron model, given the characteristic vectors;"
            , "3. class -- classify images in given directory using given model."
            ]

sourceDescr = [ "Depending on the <mode>:"
              , "\t1. arff  -- a directory with images to process;"
              , "\t2. model -- *.arff data file with classes assigned;"
              , "\t3. class -- model file to be used for classification."
              ]

targetDescr = [ "Depending on the <mode>:"
              , "\t1. arff  -- destination *.arff file;"
              , "\t2. model -- destination *.model file;"
              , "\t3. class -- *.arff file with data to classify."
              ]

-- Optionals
-----------------------------------------------------------------------------

---- 1) ARFF
--------------

saveRegions :: Optional1 Text
saveRegions = optional "" ["save-regions"]
                          ["[ARFF] Save region images, with assigned class."]
                          ["Saves root directory."]

--saveRegions = optionalFlag "" ["save-regions"]
--                           [ "[ARFF] Save region images, with assigned "
--                           ++ "class to 'reports' directory."
--                           ]

relationName :: Optional1 Text
relationName = optional "n" ["name"] ["[ARFF] Relation name."] ["Name."]

noClass = optionalFlag "?" ["no-class"]
                           ["[ARFF] Do not interrogate classes and set ?."]


---- 2) MODEL
--------------

crossValidate :: Optional1 Int
crossValidate = optional "x" ["validate"]
                             ["[MODEL] Cross validate model."]
                             ["Validation folds."]

saveReports :: Optional1 Text
saveReports = optional "" ["save-reports"]
                          ["[MODEL] Save validation reports."]
                          ["Reports directory."]

nnetSettings :: Optional1 Text
nnetSettings = optional "C" ["configure"]
                            ["[MODEL] Configure Multilayer Perceptron (see Weka)."]
                ["Configuration should be put in \"\" quotes to avoid separation."]


---- 3) CLASS
--------------








----------------------------------------------------------------------------
----------------------------------------------------------------------------


fixedRegions = FixedColRowRegions{ rRow = 50
                                 , rCol = 50
                                 , minRegionSize = (100, 100)
                                 }           -- (height, width)

instance RegionsExtractor RGB where foreachRegion = fixedColRowRegions fixedRegions



--wildfireMain reportsRoot ci = do
--          let eRGB  = extractorRGB  descriptiveStats
--              eHSV  = extractorHSV  descriptiveStats
--              eGrey = extractorGrey descriptiveStats
--
--              e1 :: CharacteristicsExtractor RGB Double (N8 :*: N3 :*: N2)
--              e1 = eRGB `combineExtractors` eHSV
--              e2 = e1 `combineExtractors` eGrey
--
--          time <- getCurrentTime
--          let reportsDir root = root ++ pathSeparator:
--                                formatTime defaultTimeLocale "%F_%T" time
--
--              save :: String -> RGB -> String -> (Int,Int) -> WildfireClass -> IO()
--              save root img nme (row,col) c =
--                let (name, ext) = splitExtension nme
--                    filename = name ++ "_" ++ show row ++ "-" ++ show col
--                            ++ "_" ++ show c ++ ext
--                in writePng (reportsDir root ++ "/" ++ filename) $ toJuicyRGB img
--              save' = fmap save reportsRoot
--          mapM_ (createDirectoryIfMissing True) reportsRoot
--
--          main' save' ci e2


-----------------------------------------------------------------------------
 ----------------------------------- EXEC ----------------------------------
-----------------------------------------------------------------------------

optsH = handleHelp "wick" header arguments


-----------------------------------------------------------------------------


mainChVec dir' arff opts verb = do
    let dir = text2str dir'
    -- Images
    imgs' <- listDirectory dir
    let imgs = map (dir </>) imgs'
    -- Optional args
        withoutClass = isJust $ opts `get` noClass
        reportsRoot = fmap text2str (opts `get` saveRegions)
        relName = maybe "wildfire" text2str (opts `get` relationName)

        target = text2str arff

    -- Characteristic Extractor
    let eRGB  = extractorRGB  descriptiveStats
        eHSV  = extractorHSV  descriptiveStats
        eGrey = extractorGrey descriptiveStats

        e1 :: CharacteristicsExtractor RGB Double (N8 :*: N3 :*: N2)
        e1 = eRGB `combineExtractors` eHSV
        e2 = e1 `combineExtractors` eGrey

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
        run ci = MainT.collectImagesCharacteristics ci e2 relName imgs target save'

    -- Run
    if withoutClass
     then run =<< (classProvider False :: IO (RegionsClassUnknown RGB WildfireClass))
     else run =<< (classProvider False :: IO (ClassesInterview    RGB WildfireClass))


-----------------------------------------------------------------------------


mainTrain s t opts verb = undefined
mainClass s t opts verb = undefined














