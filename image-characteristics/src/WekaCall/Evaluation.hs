-----------------------------------------------------------------------------
--
-- Module      :  WekaCall.Evaluation
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--



module WekaCall.Evaluation (

-- * Cross Validation
  CrossValidation(..)
, crossValidation

-- * Extract Result
, WekaReportBuilder(..)
, wekaReportBuilder'
, buildReports

-- * Reporters
, defaultReporter
, predictionsReporter

-- * Reports IO
, reportStdOut
, reportFoFile

) where

-----------------------------------------------------------------------------
-- JNI

import Weka.Core
import Weka.Classifiers


import qualified Weka.Classifiers.Evaluation as Evaluation
import qualified Weka.Core.Instances as Instances
import qualified Weka.Core.Instance  as Instance
import qualified Weka.Core.Attribute as Attribute
import qualified Weka.Core.FastVector as FastVector
import qualified Java.Util as Util
import qualified Weka.WekaCalls as Helper

-----------------------------------------------------------------------------

import JUtils

import Foreign.Java (object)
import qualified Foreign.Java as JNI
import Foreign.Java.Bindings.Support

import Control.Monad

import Data.Word
import Data.Int
import Data.Maybe (fromJust)
import Data.List (intersperse)
import Data.Array

import GHC.Float (double2Int)

-----------------------------------------------------------------------------

instance JavaClassID Util.Random'' where classId _ = "java.util.Random"

instance JavaClassID Evaluation'' where classId _ = "weka.classifiers.Evaluation"
instance JavaClassID Instances''  where classId _ = "weka.core.Instances"

-----------------------------------------------------------------------------

data (Classifier c) => CrossValidation c res = CrossValidation {
      classifier        :: c
    , validationFolds   :: Word8

    , prepareEvaluation :: Evaluation' -> Java ()
    , extractResult     :: Evaluation' -> Java res
    }



-- | Run cross validation of the instances.
crossValidation :: (Classifier c) => CrossValidation c res -> Instances' -> Java res
crossValidation v insts = do Just evalClass  <- jClass Evaluation
                             Just evalConstr <- getConstructor evalClass (object $ classId Instances)
                             obj <- asObject insts
                             Just eval' <- newObjectFrom evalConstr (Just obj)
                             Just eval <- coerce eval' Evaluation

                             prepareEvaluation v eval

                             Just jRand <- newInstance0 Util.Random

--                             JNI.toString eval >>= println
--                             JNI.toString (classifier v) >>= println
--                             JNI.toString insts >>= println
--                             println $ validationFolds v
--                             JNI.toString jRand >>= println

--                             JNI.arrayLength emptyArray >>= println

                             Helper.crossValidateModel eval (classifier v) insts
                                                            (validationFolds v) jRand :: Java ()
--                             Evaluation.crossValidateModel' eval (classifier v) insts (validationFolds v)
--                                                            jRand EmptyJArray :: Java ()

                             extractResult v eval


--evaluateModel ::


-----------------------------------------------------------------------------
-- Extract Result --


newtype WekaReportBuilder = WekaReportBuilder  [Evaluation' -> Java String]

wekaReportBuilder' :: [Evaluation' -> Java (Maybe JString)] -> WekaReportBuilder
wekaReportBuilder' = WekaReportBuilder . map ((JNI.toString . fromJust) <=<)
--WekaReportBuilder . map (flip (.) $ flip (<$>) (JNI.toString . fromJust))
--mapM (JNI.toString . fromJust)

instance Monoid WekaReportBuilder where
    mempty = WekaReportBuilder []
    mappend (WekaReportBuilder a) (WekaReportBuilder b) = WekaReportBuilder (a ++ b)

buildReports :: String -- reports separator
             -> WekaReportBuilder
             -> Evaluation'
             -> Java String
buildReports sep (WekaReportBuilder reps) ev = do
    reports <- mapM ($ ev) reps
    return $ concat reports


-----------------------------------------------------------------------------

defaultReporter = wekaReportBuilder' [ flip Evaluation.toSummaryString'' True
                                     , Evaluation.toMatrixString
                                     , Evaluation.toClassDetailsString
                                     ]

predictionsReporter = WekaReportBuilder [ Evaluation.predictions
                                      >=> ((FastVector.toArray :: FastVector' -> Java [JNI.JObject]) . fromJust)
                                      >=> mapM JNI.toString
                                      >=> return . concatMap ("\n\t\t"++)
                                      ]

-----------------------------------------------------------------------------

reportStdOut :: Java String -> Java ()
reportStdOut = (println =<<)

reportFoFile :: FilePath -> Java String -> Java ()
reportFoFile path = (io . writeFile path =<<)

-----------------------------------------------------------------------------


