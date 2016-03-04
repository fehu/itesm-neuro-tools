-----------------------------------------------------------------------------
--
-- Module      :  WekaCall.Train
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module WekaCall.Train (

  trainModel

-- * Classifiers
, multilayerPerceptron

-- * Options
, setOptions

) where

-----------------------------------------------------------------------------

import Weka.Core
import Weka.Classifiers
import Weka.Classifiers.Functions (MultilayerPerceptron''(..), MultilayerPerceptron')

import qualified Weka.Classifiers.Classifier as Classifier
import qualified Weka.WekaCalls as Helper

-----------------------------------------------------------------------------

import JUtils

import Data.Maybe (fromJust)
import Control.Monad

-----------------------------------------------------------------------------



instance JavaClassID MultilayerPerceptron''
    where classId _ = "weka.classifiers.functions.MultilayerPerceptron"

-----------------------------------------------------------------------------

-- | Train the classifier with given instances.
trainModel :: (Classifier c) => c -> Instances' -> Java ()
trainModel = Classifier.buildClassifier



setOptions :: (Classifier c) => c -> [String] -> Java ()
setOptions c = Helper.setOptions c <=< jStringList



-----------------------------------------------------------------------------
-- Classifiers --

multilayerPerceptron :: Java MultilayerPerceptron'
multilayerPerceptron = fromJust <$> newInstance0 MultilayerPerceptron





