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
    Just instances <- readDatasource src
    Just cAttr <- Instances.attribute' instances =<< jString "class"
    Instances.setClass instances cAttr :: Java ()
--    Just summary <- Instances.toSummaryString instances
--    toString summary >>= JIO.print

    c <- multilayerPerceptron
--    (MultilayerPerceptron.getTrainingTime c :: Java Int32) >>= println
--    stringResult (MultilayerPerceptron.getHiddenLayers c :: Java (Maybe JString)) >>= println
--    (MultilayerPerceptron.globalInfo c :: Java (Maybe JString)) >>= toString . fromJust >>= println

    let cv = CrossValidation {
          classifier = c
        , validationFolds = 10
        , prepareInstances = const $ return () -- filterAttributeInstance "class" ["Ignore"]
        , prepareEvaluation = const $ return ()
        , extractResult = reportStdOut . buildReports "\n\n\n" defaultReporter
        }

    crossValidation cv instances

    saveModel target "wildfire01 v.01" (classifier cv) instances

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

