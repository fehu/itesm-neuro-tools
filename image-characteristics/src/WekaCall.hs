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

  test

) where

import System.Environment
import System.FilePath

import Weka.Classifiers
import Weka.Classifiers.Functions
import Weka.Classifiers.Functions.MultilayerPerceptron as Perceptron

import qualified Foreign.Java as JNI
import qualified Foreign.Java.Bindings.Support as JNIS

import Data.Maybe
import Control.Monad

-----------------------------------------------------------------------------

test = do let addJar x = x ++ pathSeparator:"weka.jar"
          classpath <- liftM (fromMaybe ".") (fmap addJar <$> lookupEnv "WEKA_HOME")
          JNI.initJava ["-Djava.class.path=" ++ classpath]
          JNI.runJava $ do
                Just perceptronClass <- JNI.getClass "weka.classifiers.functions.MultilayerPerceptron"
                Just perceptron' <- JNI.newObject perceptronClass
                Just perceptron <- JNIS.coerce perceptron' MultilayerPerceptron

                lr <- getLearningRate perceptron :: JNI.Java Double
                JNI.io $ do putStrLn Perceptron.info'
                            putStrLn $ "LearningRate = " ++ show lr


