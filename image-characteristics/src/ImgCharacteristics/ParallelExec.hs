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
import Control.Concurrent.STM
import Control.Monad
import Control.Arrow
import Data.List (uncons)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.IORef

-----------------------------------------------------------------------------

class (Monad a) =>  IOAlias a where toIO   :: a  x -> IO x
                                    fromIO :: IO x -> a  x

instance IOAlias IO where toIO = id
                          fromIO = id

-----------------------------------------------------------------------------

type ForeachRegionM m img = forall a . img -> (img -> (Int, Int) -> m a) -> m [a]

type ForeachRegionM' m img =
    forall a . img -> ((Int, Int), (Int,Int), (img -> (Int, Int) -> m a) -> m [a])

-----------------------------------------------------------------------------

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
    parrun pool args


inParallel' :: (Eq a) =>
               Int           -- ^ Executor pool size.
            -> (a -> IO b)   -- ^ Function to be applied.
            -> [a]           -- ^ Data to apply.
            -> IO [b]
inParallel' poolSize f = inParallel poolSize (void . forkIO) (return ()) (const f)


usedThreadDelay = 10 -- millis

parrun :: (Eq a, IOAlias m) => [Executor m c a b] -> [a] -> m [b]
parrun pool args = do e <- newExecution pool args
                      parrun' e []
                      fromIO $ takeMVar $ execOutputs e


parrun' :: (Eq a, IOAlias m) => Execution m c a b -> [b] -> m ()
parrun' e acc = do
    (args, terminate) <- fromIO . atomically $ do arg <- readTVar $ execInputs e
                                                  fin <- readTVar $ execTerminate e
                                                  return (arg, fin)

    let pool = execPool e
        traversePool []      as         = return ([], as)
        traversePool (ph:pt) as = do
            let exec = writeInput ph
                finish = exec $ if terminate then ETerminate else EFinish

            out <- readOutput ph
            waits <- waitingInput ph

            case out of Just v -> do t <- case as
                                            of ah:at -> exec (EInput ah) >> return at
                                               _     -> finish           >> return []
                                     first (v:) <$> traversePool pt t
                        _      -> do t <- case as
                                            of ah:at | waits -> exec (EInput ah) >> return at
                                               []    | waits -> finish           >> return []
                                               a             -> return a
                                     traversePool pt t


    (res, args') <- fromIO $ traversePool pool args
    fromIO $ threadDelay usedThreadDelay
    let acc' = res ++ acc
    let recC = parrun' e acc'
    fromIO . atomically $ writeTVar (execInputs e) args'
    if null args then do fin <- fromIO $ liftM and $ mapM doesNothing pool
                         if fin then fromIO . putMVar (execOutputs e) $ reverse acc'
                                else recC
                 else recC

-----------------------------------------------------------------------------

data Execution m c a b = Execution {
        execPool    :: [Executor m c a b]
      , execInputs  :: TVar [a]
      , execOutputs :: MVar [b]
      , execTerminate :: TVar Bool
    }

newExecution pool args = fromIO $ do ein  <- newTVarIO args
                                     eout <- newEmptyMVar
                                     term <- newTVarIO False
                                     return $ Execution pool ein eout term

-----------------------------------------------------------------------------

data ExecutorInput a = EInput a
                     | ETerminate
                     | EFinish
                     deriving (Eq, Ord, Show)

data Executor m c a b = Executor {
        execTask   :: c -> a -> m b
      , execInput  :: MVar (ExecutorInput a)
      , execOutput :: MVar b
      , execFinished   :: TVar Bool
      , execTerminated :: TVar Bool
      , execContext      :: IORef (Maybe c)
      , execContextInit  :: m c
    }

newExecutor f init = do ma <- newEmptyMVar
                        mb <- newEmptyMVar
                        w  <- newTVarIO False
                        t  <- newTVarIO False
                        c  <- newIORef Nothing
                        return $ Executor f ma mb w t c init

writeInput = putMVar . execInput
readOutput = tryTakeMVar . execOutput
doesNothing = readTVarIO . execFinished
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

                   let setFin ps = forM_ ps $ \(sel, b) -> fromIO (atomically (writeTVar (sel e) b))

                   -- Apply to @f@ or finish.
                   case arg of EInput a -> setFin [(execFinished, False)]
                                        >> execTask e ctx a >>= (fromIO . putMVar (execOutput e))
                                        >> runExecutor e
                               EFinish    -> setFin [(execFinished, True)] >> runExecutor e
                               ETerminate -> setFin [(execFinished, True), (execTerminated, True)]


-----------------------------------------------------------------------------

