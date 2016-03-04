-----------------------------------------------------------------------------
--
-- Module      :  MainTrain
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module MainTrain where

import CArgs

import qualified Options as Opts

import JUtils
import WekaCall
import WekaCall.Train
import WekaCall.Filters
import WekaCall.Evaluation

import qualified Foreign.Java.IO as JIO

import qualified Weka.Core.Instances as Instances
import qualified Weka.Classifiers.Meta.FilteredClassifier as FC

import System.Directory

import Control.Monad

import Data.List.Split (splitOn)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Train a model and write it to file.
mainTrain src t opts verb = do
    let classAttr   = maybe "class" text2str (opts `get` Opts.classAttribute)
        modelName   = "model"
        xValidation = opts `get` Opts.crossValidate
        saveReports = fmap text2str $ opts `get` Opts.saveReports
        nnetOptions = fmap (splitOn " " . text2str) $ opts `get` Opts.nnetSettings

        classifier  = multilayerPerceptron
        target      = text2str t

    withWekaClassifier classAttr classifier (text2str src) $
        \c instances ->
            do -- Print summary
               Just summary <- Instances.toSummaryString instances
               toString summary >>= println

               -- Set classifier options
               Just c' <- FC.getClassifier c
               forM_ nnetOptions $ setOptions c'

               -- Train model
               JIO.print "\n\n\tTraining model"
               trainModel c instances
               println " finished"

               -- Save model
               saveModel target modelName c instances
               println $ "wrote model '" ++ modelName ++ "' to " ++ target

               -- Cross-Validation: Save reports
               let reportsOut rs js = let s = buildReports "\n\n\n" defaultReporter js
                                      in mapM_ ($ s) rs
                   extractResult = maybe (reportsOut [reportStdOut])
                                         (\f -> reportsOut [reportStdOut, reportFoFile f])
                                         saveReports
                   prepareEvaluation = const $ return ()

               -- Cross-Validation
               forM_ xValidation $
                \folds -> do
                    let cv = CrossValidation c (fromIntegral folds) prepareEvaluation extractResult

                    return $ mapM_ (createDirectoryIfMissing True) saveReports

                    -- The filters were applied only for the train data,
                    -- apply filters for cross-validation.
                    Just filters <- FC.getFilter c
                    instances' <- instances `applyFilter` filters

                    println "Cross validation (with 'Ignore' instances removed)"
                    crossValidation cv instances'


-----------------------------------------------------------------------------

