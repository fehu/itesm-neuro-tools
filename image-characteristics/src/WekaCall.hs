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

, loadModel

, extraClasspath
, withWekaClassifier


, makeAttrNum, makeAttrNom

, makeInstance


) where

-----------------------------------------------------------------------------

import Weka.Core (Instances', Instance''(..), Instance', Attribute')
import Weka.Classifiers (Classifier, Classifier')
import Weka.Classifiers.Meta (FilteredClassifier')
import qualified Weka.Core.Converters.ConverterUtilsDataSource as DataSource
import qualified Weka.Core.Instances as Instances
import qualified Weka.WekaCalls as Helper
import qualified Weka.Core.Instance as I
import qualified Util as Util
import qualified Util.Pair as Pair

import Java.Lang (Object)

-----------------------------------------------------------------------------

import Nat.Vec

import System.Environment
import System.FilePath

import Data.Int
import Data.Maybe
import Control.Monad
import Control.Arrow

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

-- | Load classifier model and train data header from file.
loadModel :: FilePath -> Java (Classifier', Instances')
loadModel path = do Just loaded <- Helper.loadModel =<< jString path
                    Just c <- Pair._1 loaded
                    Just i <- Pair._2 loaded
                    return (c, i)
--fmap ((Util._1 &&& Util._2) . fromJust) . Helper.loadModel <=< jString

-----------------------------------------------------------------------------

-- | Extra classpath used by project.
extraClasspath = [ "weka-calls.jar" ]

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
    Just cAttr <- Instances.attribute instances =<< jString clazz
    Instances.setClass instances cAttr :: Java ()

    -- Filters + Classifier
    nom2num <- nominalToBinary
    dropIgnore <- removeWithValues cAttr ["Ignore"]
    filters <- successiveFilters [SomeFilter dropIgnore, SomeFilter nom2num]
    c <- withFilter filters =<< c0

    f c instances

-----------------------------------------------------------------------------

makeAttrNom name domain = do
    dom   <- makeFastVector =<< mapM (fmap JObj . jString) domain
    name' <- jString name
    Just attr <- Helper.newAttrNom name' dom
    return attr

makeAttrNum name = do
    Just attr <- Helper.newAttrNum =<< jString name
    return attr

-----------------------------------------------------------------------------

instance JavaClassID Instance'' where classId _ = "weka.core.Instance"

makeInstance header vals = do
    -- Create instance and set dataset
    nAttr <- Instances.numAttributes header :: Java Int32
    Just inst <- Helper.newInstance (nAttr + 1)
    I.setDataset inst header :: Java ()

    -- Set attribute values
    let nvals = map realToFrac $ vec2list vals :: [Double]
    sequence_ $ do (a,v) <- zip ([0..] :: [Int]) nvals
                   return (I.setValue' inst a v :: Java ())

    -- Set class unknown
    I.setMissing' inst nAttr :: Java ()
    return inst


-----------------------------------------------------------------------------

