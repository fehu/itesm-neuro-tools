-----------------------------------------------------------------------------
--
-- Module      :  WekaCall.Classify
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module WekaCall.Classify (

  classify
, classifyNom

, Classifier, Classifier'

) where


import ImgCharacteristics

import JUtils

import Weka.Classifiers
import Weka.Core (Instance')

import qualified Weka.Classifiers.Classifier as C

import GHC.Float (double2Int)

-----------------------------------------------------------------------------

-- | Classify an instance.
classify :: (Classifier c) => c -> Instance' -> Java Double
classify = C.classifyInstance

-- | Classify an instance (Nominal class).
classifyNom :: (Class class', Classifier c) => c -> Instance' -> Java class'
classifyNom c i = do d <- classify c i
                     return . toEnum $ double2Int d


-----------------------------------------------------------------------------



