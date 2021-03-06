-----------------------------------------------------------------------------
--
-- Module      :  Options
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Options where

import CArgs

import qualified GHC.Read as Read
import Text.Read (readMaybe)


-- Header
-----------------------------------------------------------------------------

header = [ "\tWildfire Image Classification Kit (WICK)"
         , "\t---------------------------------  ----"
         , ""
         , "Conatains tools for 1) image characteristics extraction"
         , "                       (descriptive statistics);"
         , "                    2) Weka's Multilayer Perceptron model training;"
         , "                    3) image classification, using the model. "
         ]


-- * Mode
-----------------------------------------------------------------------------

data Mode = ARFF | MODEL | CLASS deriving Show


-- * Arguments
-----------------------------------------------------------------------------

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
      , Opt confusionTikzReport
        -- CLASS
      , Opt classificationGUI
      , Opt classifyParallel
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


-- * Positional descriptions
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
              , "\t3. class -- images directory."
              ]


-- * Optional Arguments
-----------------------------------------------------------------------------

---- 1) ARFF
--------------

saveRegions :: Optional1 Text
saveRegions = optional "" ["save-regions"]
                          ["[ARFF] Save region images, with assigned class."]
                          ["root directory"]


relationName :: Optional1 Text
relationName = optional "n" ["name"] ["[ARFF] Relation name."] ["Name."]


noClass = optionalFlag "?" ["no-class"]
                           ["[ARFF] Do not interrogate classes and set ?."]


---- 2) MODEL
--------------

crossValidate :: Optional1 Int
crossValidate = optional "x" ["validate"]
                             ["[MODEL] Cross validate model."]
                             ["validation folds"]


saveReports :: Optional1 Text
saveReports = optional "" ["x-report"]
                          ["[MODEL] Save cross validation report."]
                          ["report file"]


confusionTikzReport :: Optional1 Text
confusionTikzReport = optional "" ["tikz-confusion"]
                                  ["[MODEL] Write confusion diagram Tikz source (LaTeX)."]
                                  ["diagram file"]


nnetSettings :: Optional1 Text
nnetSettings = optional "o" ["options"]
                            ["[MODEL] Set Multilayer Perceptron options (see Weka)."]
                ["configuration should be put in \"\" quotes to avoid separation"]


---- 3) CLASS
--------------

classificationGUI = optionalFlag "G" ["gui"]
                                [ "[CLASS] Show classification results in GUI (uses GTK)."
                                , "'Return' key may be used instead of 'Next' button."
                                ]

classifyParallel :: Optional1 Int
classifyParallel = optional "" ["par"] ["[CLASS] Run classification in parallel (experimental)."]
                               [ "number of executors; the actual number of physical threads"
                               , "won't be set greater than the number of processors."]

