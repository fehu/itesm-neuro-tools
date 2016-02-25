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

) where

-----------------------------------------------------------------------------

import Weka.Core
import Weka.Classifiers
import Weka.Classifiers.Functions (MultilayerPerceptron''(..), MultilayerPerceptron')
import Weka.Classifiers.Functions.MultilayerPerceptron as Perceptron

import qualified Weka.Classifiers.Classifier as Classifier

-----------------------------------------------------------------------------

import JUtils

import Data.Maybe (fromJust)

-----------------------------------------------------------------------------



instance JavaClassID MultilayerPerceptron''
    where classId _ = "weka.classifiers.functions.MultilayerPerceptron"

-----------------------------------------------------------------------------

-- | Train the classifier with given instances.
trainModel :: (Classifier c) => c -> Instances' -> Java ()
trainModel = Classifier.buildClassifier






-----------------------------------------------------------------------------
-- Classifiers --

multilayerPerceptron :: Java MultilayerPerceptron'
multilayerPerceptron = fromJust <$> newInstance0 MultilayerPerceptron





