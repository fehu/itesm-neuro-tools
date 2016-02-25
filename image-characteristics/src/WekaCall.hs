-----------------------------------------------------------------------------
--
-- Module      :  WekaCall
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module WekaCall (

  readDatasource
, saveModel

, test

) where

-----------------------------------------------------------------------------

import Weka.Classifiers (Classifier)
import Weka.Core (Instances')
import qualified Weka.Classifiers.Functions.MultilayerPerceptron as MultilayerPerceptron
import qualified Weka.Core.Converters.ConverterUtilsDataSource as DataSource
import qualified Weka.Core.Instances as Instances
import qualified Weka.WekaCalls as Helper

-----------------------------------------------------------------------------

import System.Environment
import System.FilePath

import Data.Int
import Data.Maybe
import Control.Monad

import qualified Foreign.Java.IO as JIO

import JUtils
import WekaCall.Evaluation
import WekaCall.Filters
import WekaCall.Train

-----------------------------------------------------------------------------

--DataSource.read
readDatasource :: String -> Java (Maybe Instances')
readDatasource s = jString s >>= DataSource.read''

-----------------------------------------------------------------------------

-- | Save weka classification model to file.
saveModel :: (Classifier c) =>
             FilePath   -- ^ A path to /*.model/ target file.
                        -- ^ If the file doesn't have ".model" extension,
                        -- ^ it will be added.
          -> String     -- ^ Model name.
          -> c          -- ^ Classifier.
          -> Instances' -- ^ Data.
          -> Java ()

saveModel path name c instances = do path' <- jString path
                                     name' <- jString name
                                     Helper.saveModel path' name' c instances

-----------------------------------------------------------------------------

extraClasspath = [ "java/classes" ]

test = withWekaHomeEnv extraClasspath $ do
    let src = "wildfire01.arff"
        target = "wildfire01-01.model"
        report = "last-run.report"

    Just instances <- readDatasource src
    Just cAttr <- Instances.attribute' instances =<< jString "class"
    Instances.setClass instances cAttr :: Java ()

    Just summary <- Instances.toSummaryString instances
    toString summary >>= JIO.print

    c0 <- multilayerPerceptron

    nom2num <- nominalToBinary
    dropIgnore <- removeWithValues cAttr ["Ignore"]

    filters <- successiveFilters [SomeFilter dropIgnore, SomeFilter nom2num]

    c <- withFilter filters c0

    JIO.print "\n\n\tTraining model"
    trainModel c instances
    println " finished"

    saveModel target "wildfire01 v.01" c instances
    println $ "wrote the model to " ++ target

    let cv = CrossValidation {
          classifier = c
        , validationFolds = 10
        , prepareEvaluation = const $ return ()
        , extractResult = \js -> let s = buildReports "\n\n\n" defaultReporter js
                                 in mapM_ ($ s) [reportStdOut, reportFoFile report]
        }


    -- The filters were applied only for the train data, apply filters for cross-validation.
    instances' <- instances `applyFilter` filters
    println "Cross validation (with 'Ignore' instances removed)"
    crossValidation cv instances'


--"weka.filters.MultiFilter -F \"weka.filters.unsupervised.instance.RemoveWithValues -S 0.0 -C 57 -L 5\"
--                          -F \"weka.filters.unsupervised.attribute.NominalToBinary -R first-last\""
-- -W weka.classifiers.functions.MultilayerPerceptron -- -L 0.3 -M 0.2 -N 500 -V 0 -S 0 -E 20 -H a



--
--
--
--
--test = do Just wekaHome <- wekaHomeEnv
--          initJava wekaHome
--          runJava $ do
--                Just perceptron <- newInstance0 MultilayerPerceptron
--
--                lr <- getLearningRate perceptron :: Java Double
--                io $ putStrLn $ "LearningRate = " ++ show lr


--test = do

