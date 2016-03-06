-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.ParallelExec
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}


module ImgCharacteristics.ParallelExec (

  ForeachRegionM
, RegionsExtractorPar(..)
, IOAlias(..)

, inParallel

) where

import ImgCharacteristics

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Arrow
import Data.List (uncons)
import Data.Maybe (fromMaybe, isJust)

-----------------------------------------------------------------------------

type ForeachRegionM m img = forall a . img -> (img -> (Int, Int) -> m a) -> m [a]

type ForeachRegionM' m img =
    forall a . img -> ((Int, Int), (Int,Int), (img -> (Int, Int) -> m a) -> m [a])

class IOAlias a where toIO   :: a  x -> IO x
                      fromIO :: IO x -> a  x

-- | Process each region in parallell.
class RegionsExtractorPar m img where foreachRegionPar  :: Int -- ^ Number of threads.
                                                        -> ForeachRegionM m img
                                      foreachRegionPar' :: Int -> ForeachRegionM' m img

instance (RegionsExtractor img, Eq img, IOAlias m) => RegionsExtractorPar m img where
    foreachRegionPar  poolSize img f = fromIO $ inParallel poolSize
                                                          (toIO . uncurry f)
                                                          (foreachRegion img (,))
    foreachRegionPar' poolSize img = let (rc, rs, _) = foreachRegion' img
                                     in (rc, rs, foreachRegionPar poolSize img)

-----------------------------------------------------------------------------

inParallel :: (Eq a) =>
              Int           -- ^ Executor pool size.
           -> (a -> IO b)   -- ^ Function to be applied.
           -> [a]           -- ^ Data to apply.
           -> IO [b]
inParallel poolSize f args = do
    pool <- replicateM poolSize (newExecutor f)
    forM_ pool forkExecutor
    parrun pool args []


usedThreadDelay = 10 -- millis

parrun :: (Eq a) => [Executor a b] -> [a] -> [b] -> IO [b]
parrun pool args acc =

    let traversePool []      as         = return ([], as)
        traversePool (ph:pt) as = do
            let exec = writeInput ph

            out <- readOutput ph
            waits <- waitingInput ph

            case out of Just v -> do t <- case as
                                            of ah:at -> exec (EInput ah) >> return at
                                               _     -> exec EFinish     >> return []
                                     first (v:) <$> traversePool pt t
                        _      -> do t <- case as
                                            of ah:at | waits -> exec (EInput ah) >> return at
                                               []    | waits -> exec EFinish     >> return []
                                               a             -> return a
                                     traversePool pt t


    in do (res, args') <- traversePool pool args
          threadDelay usedThreadDelay
          let acc' = res ++ acc
          let recC = parrun pool args' acc'
          if null args then do fin <- liftM and $ mapM doesNothing pool
                               if fin then return $ reverse acc'
                               else recC
                       else recC


-----------------------------------------------------------------------------

data ExecutorInput a = EInput a
                     | EFinish
                     deriving (Eq, Ord, Show)

data Executor a b = Executor (a -> IO b) (MVar (ExecutorInput a)) (MVar b)

newExecutor f = do ma <- newEmptyMVar
                   mb <- newEmptyMVar
                   return $ Executor f ma mb

writeInput   (Executor _ ma _) = putMVar ma
readOutput   (Executor _ _ mb) = tryTakeMVar mb
doesNothing  (Executor _ ma _) = do out <- tryReadMVar ma
                                    return $ out == Just EFinish
waitingInput (Executor _ ma _) = isEmptyMVar ma

runExecutor e@(Executor f ma mb) = do -- Wait for the argument
                                      arg <- takeMVar ma
                                      -- Apply to @f@ or finish.
                                      case arg of EInput a -> f a >>= putMVar mb
                                                                  >> runExecutor e
                                                  EFinish  -> void $ putMVar ma EFinish

forkExecutor = forkIO . runExecutor

-----------------------------------------------------------------------------

