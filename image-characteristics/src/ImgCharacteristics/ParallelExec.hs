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
, inParallel'

) where

import ImgCharacteristics

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Arrow
import Data.List (uncons)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.IORef

-----------------------------------------------------------------------------

type ForeachRegionM m img = forall a . img -> (img -> (Int, Int) -> m a) -> m [a]

type ForeachRegionM' m img =
    forall a . img -> ((Int, Int), (Int,Int), (img -> (Int, Int) -> m a) -> m [a])

class (Monad a) =>  IOAlias a where toIO   :: a  x -> IO x
                                    fromIO :: IO x -> a  x

-- | Process each region in parallell.
class RegionsExtractorPar m img where foreachRegionPar  :: Int -- ^ Number of threads.
                                                        -> ForeachRegionM m img
                                      foreachRegionPar' :: Int -> ForeachRegionM' m img

instance (RegionsExtractor img, Eq img, IOAlias m) => RegionsExtractorPar m img where
    foreachRegionPar  poolSize img f = fromIO $ inParallel' poolSize
                                                          (toIO . uncurry f)
                                                          (foreachRegion img (,))
    foreachRegionPar' poolSize img = let (rc, rs, _) = foreachRegion' img
                                     in (rc, rs, foreachRegionPar poolSize img)

-----------------------------------------------------------------------------

inParallel :: (Eq a, IOAlias m) =>
              Int               -- ^ Executor pool size.
           -> (m () -> m ())    -- ^ Fork executor thread.
           -> m c               -- ^ Initialize context.
           -> (c -> a -> m b)  -- ^ Function to be applied (with context).
           -> [a]               -- ^ Data to apply.
           -> m [b]
inParallel poolSize forkExecutor init f args = do
    pool <- replicateM poolSize (fromIO $ newExecutor f init)
    forM_ pool (forkExecutor . runExecutor)
    parrun pool args []


inParallel' :: (Eq a) =>
               Int           -- ^ Executor pool size.
            -> (a -> m b)   -- ^ Function to be applied.
            -> [a]           -- ^ Data to apply.
            -> m [b]
inParallel' poolSize f = undefined -- inParallel poolSize (return ()) (const f)


usedThreadDelay = 10 -- millis

parrun :: (Eq a, IOAlias m) => [Executor m c a b] -> [a] -> [b] -> m [b]
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


    in do (res, args') <- fromIO $ traversePool pool args
          fromIO $ threadDelay usedThreadDelay
          let acc' = res ++ acc
          let recC = parrun pool args' acc'
          if null args then do fin <- fromIO $ liftM and $ mapM doesNothing pool
                               if fin then return $ reverse acc'
                               else recC
                       else recC


-----------------------------------------------------------------------------

data ExecutorInput a = EInput a
                     | EFinish
                     deriving (Eq, Ord, Show)

data Executor m c a b = Executor {
        execTask    :: c -> a -> m b
      , execInput   :: MVar (ExecutorInput a)
      , execOutput  :: MVar b
      , execContext     :: IORef (Maybe c)
      , execContextInit :: m c
    }

newExecutor f init = do ma <- newEmptyMVar
                        mb <- newEmptyMVar
                        c  <- newIORef Nothing
                        return $ Executor f ma mb c init

writeInput = putMVar . execInput
readOutput = tryTakeMVar . execOutput
doesNothing e = (Just EFinish ==) <$> tryReadMVar (execInput e)
waitingInput = isEmptyMVar . execInput


runExecutor :: (IOAlias m) => Executor m c a b -> m ()
runExecutor e = do -- Initialize if needed
                   ctx' <- fromIO . readIORef $ execContext e
                   ctx  <- case ctx' of Just c -> return c
                                        _      -> do c <- execContextInit e
                                                     fromIO $ writeIORef (execContext e) (Just c)
                                                     return c
                   -- Wait for the argument
                   arg <- fromIO $ takeMVar $ execInput e

                   -- Apply to @f@ or finish.
                   case arg of EInput a -> execTask e ctx a >>= (fromIO . putMVar (execOutput e))
                                                            >> runExecutor e
                               EFinish  -> fromIO . void $ putMVar (execInput e) EFinish


-----------------------------------------------------------------------------

