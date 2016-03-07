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

  ForeachRegionPar
, RegionsExtractorPar(..)
, IOLike(..)

, inParallel
--, inParallel'

, Execution
, parNew
, parExec
, parFork
, parForked

, parAppendArgs
, parGet
, parWait
, parTerminate

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
import Data.Sequence (Seq, viewl, ViewL(..), (><))
import qualified Data.Sequence as Seq

-----------------------------------------------------------------------------

class (Monad a) =>  IOLike a where toIO   :: a  x -> IO x
                                   fromIO :: IO x -> a  x

instance IOLike IO where toIO = id
                         fromIO = id

-----------------------------------------------------------------------------

--type ForeachRegionM m img = forall a . img -> (img -> (Int, Int) -> m a) -> m [a]

--type ForeachRegionM' m img =
--    forall a . img -> ((Int, Int), (Int,Int), (img -> (Int, Int) -> m a) -> m [a])

-----------------------------------------------------------------------------

type ForeachRegionPar m c img r = Execution m c (img, (Int, Int)) r -> m [r]

-- | Process each region in parallell.
class RegionsExtractorPar m img where foreachRegionPar  :: img -> ForeachRegionPar m c img r

                                      foreachRegionPar' :: img -> ( RegionsCount
                                                                  , RegionsSize
                                                                  , ForeachRegionPar m c img r
                                                                  )

instance (RegionsExtractor img, Eq img, IOLike m) => RegionsExtractorPar m img where

    foreachRegionPar img ex = do parFork ex
                                 parAppendArgs ex regions
                                 parWait ex
        where regions = foreachRegion img (,)

    foreachRegionPar' img = (rCount, rSize, foreachRegionPar img)
        where (rCount, rSize, _) = foreachRegion' img


-----------------------------------------------------------------------------

-- | Run in parallel.
inParallel :: (Eq a, IOLike m) =>
              Int               -- ^ Executor pool size.
           -> (m () -> m ())    -- ^ Fork executor thread.
           -> m c               -- ^ Initialize context.
           -> (c -> a -> m b)  -- ^ Function to be applied (with context).
           -> [a]               -- ^ Data to apply.
           -> m [b]
inParallel poolSize forkExecutor init f args =
    parExec =<< parNew poolSize forkExecutor init f args


-----------------------------------------------------------------------------

-- | Creates new 'Execution'.
parNew :: (IOLike m) =>
          Int               -- ^ Pool size.
       -> (m () -> m ())    -- ^ Fork executor thread.
       -> m c               -- ^ Init context in 'Executor' thread.
       -> (c -> a -> m b)   -- ^ Executor's task function.
       -> [a]               -- ^ Initial input arguments.
       -> m (Execution m c a b)
parNew poolSize forkExecutor init f args = do
    pool <- replicateM poolSize (fromIO $ newExecutor f init)
    forM_ pool (forkExecutor . runExecutor)
    newExecution pool (Seq.fromList args)


-- | Executes controller in current thread and terminates it.
parExec :: (IOLike m, Eq a) => Execution m c a b -> m [b]
parExec e = do -- Single execution; terminate after that.
               parTerminate e
               parrun e []
               fromIO . takeMVar $ execOutputs e

-- | Executes controller in a separate thread, if not marked as forked.
--   Marks as forked.
parFork :: (IOLike m, Eq a) => Execution m c a b -> m ()
parFork e = do forked <- parForked e
               unless forked $ fromIO . void . forkIO
                             $ do atomically $ writeTVar (execForked e) True
                                  toIO $ parrun e []

--fromIO . void . forkIO $ do atomically $ writeTVar (execForked e) True
--                                        toIO $ parrun e []

-- Marked as forked?
parForked :: (IOLike m) => Execution m c a b -> m Bool
parForked = fromIO . readTVarIO . execForked

-- | Append input arguments list for execution.
parAppendArgs :: (IOLike m) => Execution m c a b -> [a] -> m ()
parAppendArgs e = parAppendArgsSeq e . Seq.fromList
--fromIO . atomically . modifyTVar' (execInputs e) . flip (><) . Seq.fromList


-- | Append input arguments 'Seq' for execution.
parAppendArgsSeq :: (IOLike m) => Execution m c a b -> Seq a -> m ()
parAppendArgsSeq e = fromIO . atomically . modifyTVar' (execInputs e) . flip (><)


-- | Wait results. Blocks.
parWait :: (IOLike m) => Execution m c a b -> m [b]
parWait = fromIO . takeMVar . execOutputs

-- | Try to read results. Returns immediately.
parGet :: (IOLike m) => Execution m c a b -> m (Maybe [b])
parGet = fromIO . tryTakeMVar . execOutputs

-- | Marks 'Execution' for termination. Returns immediately.
--   Liberates threads after all arguments hace been processed.
parTerminate :: (IOLike m) => Execution m c a b -> m ()
parTerminate = fromIO . atomically . flip writeTVar True . execTerminate

-----------------------------------------------------------------------------

usedThreadDelay = 10 -- millis

-----------------------------------------------------------------------------

parrun :: (Eq a, IOLike m) => Execution m c a b -> [b] -> m ()
parrun ex acc = do
    (argsSeq, terminate) <- fromIO . atomically $ do
                                        arg <- readTVar $ execInputs ex
                                        fin <- readTVar $ execTerminate ex
                                        return (arg, fin)

    let traversePool :: (Eq a) => [Executor m c a b] -> Seq a -> IO ([b], Seq a)
        traversePool []     args = return ([], args)
        traversePool (e:et) args = do
            let exec = writeInput e
                next' = traversePool et
                next v t = first (v:) <$> next' t

            term <- terminated e
            fin  <- finished   e
            out  <- readOutput e
            if term then traversePool et args
                    else if fin
                            then case viewl args        of a :< at           -> exec (EInput a) >>
                                                                                next' at
                                                           _ | terminate     -> exec ETerminate >>
                                                                                next' args
                                                           _                 -> next' args

                            else case (viewl args, out) of (a :< at, Just v) -> exec (EInput a) >>
                                                                                next v at
                                                           (_,       Just v) -> exec EFinish    >>
                                                                                next v args
                                                           _                 -> next' args
    let pool = execPool ex

    (res, args') <- fromIO $ traversePool pool argsSeq

    let used = length argsSeq - length args'
        acc' = res ++ acc
    -- Drop used args
    unless (used == 0) $ fromIO . atomically $ modifyTVar (execInputs ex) (Seq.drop used)

    allFin <- and <$> forM pool (fromIO . finished)

    if null argsSeq && allFin then do unless (null acc') . fromIO $ -- Put return value
                                             putMVar (execOutputs ex) (reverse acc')
                                      allTerm <- and <$> forM pool (fromIO . terminated)
                                      -- Rec call if not terminating yet
                                      unless (terminate && allTerm) $ parrun ex []
                              else parrun ex acc'

-----------------------------------------------------------------------------

data Execution m c a b = Execution {
        execPool    :: [Executor m c a b]
      , execInputs  :: TVar (Seq a)
      , execOutputs :: MVar [b]
      , execTerminate :: TVar Bool
      , execForked    :: TVar Bool
    }

newExecution pool args = fromIO $ do ein  <- newTVarIO args
                                     eout <- newEmptyMVar
                                     term <- newTVarIO False
                                     fork <- newTVarIO False
                                     return $ Execution pool ein eout term fork

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
      , execContext      :: IORef (Maybe c)
      , execContextInit  :: m c
    }

newExecutor f init = do ma <- newEmptyMVar
                        mb <- newEmptyMVar
                        w  <- newTVarIO True
                        c  <- newIORef Nothing
                        return $ Executor f ma mb w c init

-----------------------------------------------------------------------------

writeInput = putMVar . execInput
readOutput = tryTakeMVar . execOutput
finished = readTVarIO . execFinished

terminated :: (Eq a) => Executor m c a b -> IO Bool
terminated = fmap (Just ETerminate ==) . tryReadMVar . execInput

-- | Execution loop.
--   - EInput     -> exec, put result, read input (rec).
--   - EFinish    -> read input (rec).
--   - ETerminate -> exit loop.
--
--  States:
--   - Working          -- input MVar is empty, finished flag not set.
--   - Finished (Ready) -- input MVar is empty, finished flag set.
--   - Terminated       -- input MVar is set to 'ETerminate' (finished flag also set).
runExecutor :: (IOLike m) => Executor m c a b -> m ()
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
                               ETerminate -> setFin [(execFinished, True)] >>
                                             fromIO (putMVar (execInput e) ETerminate)


-----------------------------------------------------------------------------

