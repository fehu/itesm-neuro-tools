
module Main where

import ImgCharacteristics.ParallelExec
import JUtils

import qualified Java.Lang.Integer as I
import qualified Foreign.Java as J

import Data.Int
import Control.Monad

main = runJava $ inParallel 2 (void . J.forkJava) init f ([10..20] :: [Int32])
    where init :: Java (Java a -> Java a)
          init = fromIO . fmap (fromIO .) $ withWekaHomeEnv' extraClasspath
          f :: (Java () -> Java ()) -> Int32 -> Java ()
          f c v = c $ do Just s <- I.toHexString v
                         println =<< toString s


extraClasspath = [ "java/classes" ]
