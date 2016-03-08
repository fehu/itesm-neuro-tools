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
, wekaReportBuilder
, wekaReportBuilder'
, buildReports

-- * Reporters ops

, zipReporters

-- * Reporters

, defaultReporter
, predictionsReporter
, confusionReporter

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
import qualified Util.MatrixWrapper as MW
import qualified Weka.WekaCalls as Helper
import Java.Lang.Double (doubleValue)
import Java.Lang (Double')
import Util (MatrixWrapper')

-----------------------------------------------------------------------------

import JUtils

import Foreign.Java (object)
import qualified Foreign.Java as JNI
import Foreign.Java.Bindings.Support

import Control.Monad

import Data.Word
import Data.Int
import Data.Maybe (fromJust)
import Data.List (intercalate)
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

                             Helper.crossValidateModel eval (classifier v) insts
                                                            (validationFolds v) jRand :: Java ()


                             extractResult v eval


-----------------------------------------------------------------------------
-- Extract Result --


newtype WekaReportBuilder r = WekaReportBuilder  (Evaluation' -> Java r)

wekaReportBuilder :: [Evaluation' -> Java r] -> WekaReportBuilder [r]
wekaReportBuilder es = WekaReportBuilder $ \e -> mapM ($ e) es


wekaReportBuilder' :: [Evaluation' -> Java (Maybe r)] -> WekaReportBuilder [r]
wekaReportBuilder' = wekaReportBuilder . map ((return . fromJust) <=<)

instance Functor WekaReportBuilder where
    fmap f (WekaReportBuilder gs) = WekaReportBuilder (fmap f . gs)


buildReports :: WekaReportBuilder r -> Evaluation' -> Java r
buildReports (WekaReportBuilder f) = f

zipReporters :: WekaReportBuilder a -> WekaReportBuilder b -> WekaReportBuilder (a,b)
zipReporters (WekaReportBuilder a) (WekaReportBuilder b) =
    WekaReportBuilder $ \e -> (,) <$> a e <*> b e


-----------------------------------------------------------------------------

defaultReporter = wekaReportBuilder' [ flip Evaluation.toSummaryString'' True
                                     , Evaluation.toMatrixString'
                                     , Evaluation.toClassDetailsString'
                                     ]

confusionReporter :: WekaReportBuilder [[Double]]
confusionReporter = WekaReportBuilder $ (toList . fromJust) <=< Helper.wrappedConfusionMatrix
    where toList :: MatrixWrapper' Double' -> Java [[Double]]
          toList m = do d1 <- MW.dim1 m
                        d2 <- MW.dim2 m
                        sequence $ do i <- [0 .. d1-1] :: [Int32]
                                      return . sequence
                                             $ do j <- [0 .. d2-1] :: [Int32]
                                                  return $ MW.get m i j >>= doubleValue . fromJust


predictionsReporter = wekaReportBuilder [
        Evaluation.predictions
    >=> ((FastVector.toArray :: FastVector' -> Java [JNI.JObject]) . fromJust)
    >=> mapM JNI.toString
    >=> return . concatMap ("\n\t\t"++)
  ]

-----------------------------------------------------------------------------

rsep = replicate 3 '\n'

reportStdOut :: Java [String] -> Java ()
reportStdOut = (println . intercalate rsep =<<)

reportFoFile :: FilePath -> Java [String] -> Java ()
reportFoFile path = (io . writeFile path . intercalate rsep =<<)

-----------------------------------------------------------------------------


