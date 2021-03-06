-----------------------------------------------------------------------------
--
-- Module      :  JUtils
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleInstances, UndecidableInstances, ExistentialQuantification #-}

module JUtils (

  initJava
, wekaHomeEnv
, withWekaHomeEnv
, withWekaHomeEnv'


, JavaClassID(..)
, newInstance0

, JString
, jString
, stringResult

, JList
, jObjectsList
, jStringList
, appendToJList

, JObj(..)
, makeFastVector

, JNI.Java
, JNI.io
, JNI.runJava

, JNI.newObjectFrom
, JNI.getConstructor

, JNI.JavaObject(..)

, JIO.println

) where

-----------------------------------------------------------------------------

import qualified Foreign.Java as JNI
import qualified Foreign.Java.Bindings.Support as JNIS
import qualified Java.Lang as Lang
import qualified Foreign.Java.IO as JIO
import qualified Java.Util.List as JList
import qualified Weka.WekaCalls as Helper
import qualified Weka.Core.FastVector as FV

import Weka.Core (FastVector', FastVector''(..))
import Java.Util (List')
import Java.Lang (Object)

-----------------------------------------------------------------------------

import ImgCharacteristics.ParallelExec

import System.Environment
import System.FilePath

import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.List (intercalate)

-----------------------------------------------------------------------------

instance IOLike JNI.Java where toIO   = JNI.runJava
                               fromIO = JNI.io

-----------------------------------------------------------------------------

instance JavaClassID FastVector'' where classId _ = "weka.core.FastVector"

-----------------------------------------------------------------------------

-- | Inits JVM, given a path to WEKA root directory.
initJava wekaHome extraClassPath = do
    let wClasspath = wekaHome ++ pathSeparator:"weka.jar"
        classpath = intercalate [searchPathSeparator] (wClasspath:extraClassPath)
    JNI.initJava ["-Djava.class.path=" ++ classpath]


-- | Get @WEKA_HOME@ environment variable.
wekaHomeEnv = lookupEnv "WEKA_HOME"

withWekaHomeEnv :: [String] -> JNI.Java a -> IO a
withWekaHomeEnv extraClasspath java = ($ java) =<< withWekaHomeEnv' extraClasspath

withWekaHomeEnv' :: [String] -> IO (JNI.Java a -> IO a)
withWekaHomeEnv' extraClasspath = do
                          Just wekaHome <- wekaHomeEnv
                          initJava wekaHome extraClasspath
                          return JNI.runJava

-----------------------------------------------------------------------------

-- | Create new instance, using the default (no argument) constructor.
newInstance0 :: (JNIS.InstanceOf a, JNIS.CoercedType a ~ o, JavaClassID a) =>
             a      -- ^ An identifier, generated by `j2hs`.
             -> JNI.Java (Maybe o)

newInstance0 i = do Just clazz <- JNI.getClass $ classId i
                    Just inst  <- JNI.newObject clazz
                    JNIS.coerce inst i


-- | Full class identifier.
class JavaClassID a where classId :: a -> String
                          jClass  :: a -> JNI.Java (Maybe JNI.JClass)

                          jClass = JNI.getClass . classId

-----------------------------------------------------------------------------

type JString = Lang.String'
--type JString = Lang.String''

jString :: String -> JNI.Java JString
jString s = do Just clazz <- JNI.getClass "java.lang.String"
               Just strConstructor <- JNI.getConstructor clazz JNI.string
               Just jstr' <- JNI.newObjectFrom strConstructor s
               Just jstr <- JNIS.coerce jstr' Lang.String
               return jstr

-----------------------------------------------------------------------------


type JList  = List' JNI.JObject

instance Lang.Object JNI.JObject

instance JNIS.ObjectResult JList where
    toObjectResult (Right (Just obj)) = JNIS.unsafeFromJObject obj
--Either JThrowable (Maybe JObject) -> Java m

appendToJList :: JList -> [JNI.JObject] -> JNI.Java JList
appendToJList jList objs = do vs <- mapM (JList.add jList :: JNI.JObject -> JNI.Java Bool) objs
                              return jList

jObjectsList :: [JNI.JObject] -> JNI.Java JList
jObjectsList objs = do Just jList <- Helper.newList
                       appendToJList jList objs

jStringList :: [String] -> JNI.Java (List' JString)
jStringList strs = do jStrs <- mapM jString strs
                      Just jList <- Helper.newList
                      vs <- mapM (JList.add jList :: JString -> JNI.Java Bool) jStrs
                      return jList

-----------------------------------------------------------------------------

stringResult :: JNI.Java (Maybe JString) -> JNI.Java String
stringResult a = do Just res <- a
                    JNI.toString res

-----------------------------------------------------------------------------

instance JNIS.ArrayResult [JNI.JObject] where
    type ArrayResultType [JNI.JObject] = JNIS.ArrayResultType [String]
    type ArrayResultComponent [JNI.JObject] = Maybe JNI.JObject

    toArrayResult (Right (Just arr)) = liftM (map fromJust) (JNI.toList arr)


-----------------------------------------------------------------------------

data JObj = forall o . (Object o) => JObj o

instance JNI.JavaObject JObj where asObject (JObj o) = JNI.asObject o
instance Object JObj

makeFastVector :: [JObj] -> JNI.Java FastVector'
makeFastVector objs = do
    Just fv <- newInstance0 FastVector
    mapM_  (FV.addElement fv :: JObj -> JNI.Java ()) objs
    return fv




