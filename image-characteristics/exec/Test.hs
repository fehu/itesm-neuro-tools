
module Main where

import ImgCharacteristics.ParallelExec
import JUtils

import qualified Java.Lang.Integer as I
import qualified Foreign.Java as J

import Data.Int
import Control.Monad

main = runJava $ inParallel 2 (void . J.forkJava) init f ([10..20] :: [Int32])
    where init :: Java (Java a -> Java a, [String])
          init = fromIO $ do g <- (fromIO .) <$> withWekaHomeEnv' extraClasspath
                             return (g, [])

          f (f,_) v = f $ do Just s <- I.toHexString v
                             println =<< toString s


extraClasspath = [ "java/classes" ]
