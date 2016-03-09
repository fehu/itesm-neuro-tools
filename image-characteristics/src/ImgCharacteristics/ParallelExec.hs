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

, Execution
, parNew
, parExec
, parFork
, parForked

, parAppendArgs
, parGet
, parWait
, parWaitLen
, parTerminate

) where

import ImgCharacteristics

import Control.Concurrent (forkOS)
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

type ForeachRegionPar m c img r = Execution m c (img, (Int, Int)) r -> m [r]

-- | Process each region in parallell.
class RegionsExtractorPar m img where -- foreachRegionPar  :: img -> ForeachRegionPar m c img r

                                      foreachRegionPar' :: img -> ( RegionsCount
                                                                  , RegionsSize
                                                                  , ForeachRegionPar m c img r
                                                                  )

instance (RegionsExtractor img, Eq img, IOLike m) => RegionsExtractorPar m img where
    foreachRegionPar' img = (rCount, rSize, foreachRegionPar img)
        where (rCount, rSize, _) = foreachRegion' img
              totalLength        = uncurry (*) rCount
              foreachRegionPar img ex = do parFork ex
                                           fromIO $ putStrLn $ "rCount = " ++ show rCount
                                           sequence_ $ foreachRegion img (curry (parAppendArg ex))
                                           parWaitLen ex totalLength


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
               parrun e
               fromIO . atomically . takeTMVar $ execOutputs e

-- | Executes controller in a separate thread, if not marked as forked.
--   Marks as forked.
parFork :: (IOLike m, Eq a) => Execution m c a b -> m ()
parFork e = do forked <- parForked e
               unless forked $ fromIO . void . forkOS
                             $ do atomically $ writeTVar (execForked e) True
                                  toIO $ parrun e

-- Marked as forked?
parForked :: (IOLike m) => Execution m c a b -> m Bool
parForked = fromIO . readTVarIO . execForked

-- | Append input argument for execution.
parAppendArg :: (IOLike m) => Execution m c a b -> a -> m ()
parAppendArg e = parAppendArgsSeq e . Seq.singleton


-- | Append input arguments list for execution.
parAppendArgs :: (IOLike m) => Execution m c a b -> [a] -> m ()
parAppendArgs e = parAppendArgsSeq e . Seq.fromList


-- | Append input arguments 'Seq' for execution.
parAppendArgsSeq :: (IOLike m) => Execution m c a b -> Seq a -> m ()
parAppendArgsSeq e = fromIO . atomically . modifyTVar' (execInputs e) . flip (><)


-- | Wait results. Blocks.
parWait :: (IOLike m) => Execution m c a b -> m [b]
parWait = fromIO . atomically . takeTMVar . execOutputs

-- | Wait result of minimal length. Blocks.
parWaitLen :: (IOLike m) => Execution m c a b -> Int -> m [b]
parWaitLen ex len = do len' <- fromIO . atomically $ length <$> readTMVar (execOutputs ex)
                       if len' >= len then parWait ex
                                      else parWaitLen ex len

-- | Try to read results. Returns immediately.
parGet :: (IOLike m) => Execution m c a b -> m (Maybe [b])
parGet = fromIO . atomically . tryTakeTMVar . execOutputs

-- | Marks 'Execution' for termination. Returns immediately.
--   Liberates threads after all arguments hace been processed.
parTerminate :: (IOLike m) => Execution m c a b -> m ()
parTerminate = fromIO . atomically . flip writeTVar True . execTerminate

-----------------------------------------------------------------------------

usedThreadDelay = 0 -- millis

-----------------------------------------------------------------------------

parrun :: (Eq a, IOLike m) => Execution m c a b -> m ()
parrun ex = do
    (argsSeq, terminate) <- fromIO . atomically $ do
                                        arg <- readTVar $ execInputs ex
                                        fin <- readTVar $ execTerminate ex
                                        return (arg, fin)

    let traversePool :: (Eq a) => [Executor m c a b] -> Seq a -> IO ([b], Seq a)
        traversePool []     args = return ([], args)
        traversePool (e:et) args = do
            let exec = atomically . writeInput e
                next' = traversePool et
                next v t = first (v:) <$> next' t

            (out,term,w)  <- atomically $ (,,) <$> readOutput e
                                               <*> terminated e
                                               <*> waitsInput e
            if term then next' args
                    else case (viewl args, out) of (a :< at, Just v) -> exec (EInput a) >>
                                                                        next v at
                                                   (_,       Just v) -> next v args
                                                   (a :< at, _) | w  -> exec (EInput a) >>
                                                                        next' at
                                                   _                 -> when terminate
                                                                             (exec ETerminate) >>
                                                                        next' args
    let pool = execPool ex

    (res, args') <- fromIO $ traversePool pool argsSeq

    let used = length argsSeq - length args'
        guard = fromIO . atomically $ do
                    emptyRes <- isEmptyTMVar (execOutputs ex)
                    ($ execOutputs ex)
                              $ if emptyRes
                                then flip putTMVar res
                                else \v -> takeTMVar v >>= putTMVar v . (res ++)

    -- Drop used args
    unless (used == 0) $ fromIO . atomically $ modifyTVar (execInputs ex) (Seq.drop used)

    unless (null res) . fromIO $ guard -- Put return value
    allTerm <- fromIO . atomically $ and <$> forM pool terminated
    -- Rec call if not terminating yet
    unless (terminate && allTerm) $ parrun ex


-----------------------------------------------------------------------------

data Execution m c a b = Execution {
        execPool    :: [Executor m c a b]
      , execInputs  :: TVar (Seq a)
      , execOutputs :: TMVar [b]
      , execTerminate :: TVar Bool
      , execForked    :: TVar Bool
    }

newExecution pool args = fromIO $ do ein  <- newTVarIO args
                                     eout <- atomically newEmptyTMVar
                                     term <- newTVarIO False
                                     fork <- newTVarIO False
                                     return $ Execution pool ein eout term fork

-----------------------------------------------------------------------------

data ExecutorInput a = EInput a
                     | ETerminate
                     deriving (Eq, Ord, Show)

data Executor m c a b = Executor {
        execTask   :: c -> a -> m b
      , execInput  :: TMVar (ExecutorInput a)
      , execOutput :: TMVar b
      , execContext      :: IORef (Maybe c)
      , execContextInit  :: m c
    }

newExecutor f init = do ma <- newEmptyTMVarIO
                        mb <- newEmptyTMVarIO
                        c  <- newIORef Nothing
                        return $ Executor f ma mb c init

-----------------------------------------------------------------------------

writeInput = putTMVar . execInput
readOutput = tryTakeTMVar . execOutput
waitsInput = isEmptyTMVar . execInput

terminated :: (Eq a) => Executor m c a b -> STM Bool
terminated = fmap (Just ETerminate ==) . tryReadTMVar . execInput

-- | Execution loop.
--   - EInput     -> exec, put result, read input (rec).
--   - ETerminate -> exit loop.
--
--  States:
--   - Working          -- input TMVar is empty, finished flag not set.
--   - Terminated       -- input TMVar is set to 'ETerminate' (finished flag also set).
runExecutor :: (IOLike m) => Executor m c a b -> m ()
runExecutor e = do -- Initialize if needed
                   ctx' <- fromIO . readIORef $ execContext e
                   ctx  <- case ctx' of Just c -> return c
                                        _      -> do c <- execContextInit e
                                                     fromIO $ writeIORef (execContext e) (Just c)
                                                     return c
                   -- Wait for the argument
                   arg <- fromIO .atomically $ takeTMVar $ execInput e

                   -- Apply to @f@ or finish.
                   case arg of EInput a   -> execTask e ctx a
                                          >>= (fromIO . atomically . putTMVar (execOutput e))
                                          >> runExecutor e
                               ETerminate -> fromIO (atomically $ putTMVar (execInput e) ETerminate)


-----------------------------------------------------------------------------

