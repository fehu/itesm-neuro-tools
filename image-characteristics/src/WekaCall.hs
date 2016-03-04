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

, extraClasspath
, withWekaClassifier

) where

-----------------------------------------------------------------------------

import Weka.Core (Instances')
import Weka.Classifiers (Classifier)
import Weka.Classifiers.Meta (FilteredClassifier')
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

-- | Extra classpath used by project.
extraClasspath = [ "java/classes" ]

type WithWekaClassifier r = FilteredClassifier' -> Instances' -> Java r

-- | Call weka, using @WEKA_HOME@ environmental variable.
-- | Links classifier with 'nom2num' and 'dropIgnore' filters.
withWekaClassifier :: (Classifier c) =>
                      String                -- ^ Class attribute name.
                   -> Java c                -- ^ Create classifier.
                   -> String                -- ^ *.arff file.
                   -> WithWekaClassifier r  -- ^ The action.
                   -> IO r

withWekaClassifier clazz c0 src f = withWekaHomeEnv extraClasspath $ do
    -- Instances
    Just instances <- readDatasource src
    Just cAttr <- Instances.attribute' instances =<< jString clazz
    Instances.setClass instances cAttr :: Java ()

    -- Filters + Classifier
    nom2num <- nominalToBinary
    dropIgnore <- removeWithValues cAttr ["Ignore"]
    filters <- successiveFilters [SomeFilter dropIgnore, SomeFilter nom2num]
    c <- withFilter filters =<< c0

    f c instances

-----------------------------------------------------------------------------

