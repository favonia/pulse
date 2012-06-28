{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  Sound.Pulse.Monad.Internal.MainLoop
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

High-level interface for mainloop.
-}

module Sound.Pulse.Monad.Internal.MainLoop
    (
    -- * High-level main loop
    MainLoop,
    getApi,
    newLoop,
    freeLoop,
    runLoop,
    quitLoop,
    -- * Blocking
    blockLoop,
    -- * Operation registering
    wrapRawOp,
    OperationState,
    ) where

import Data.Maybe (catMaybes)
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

import Sound.Pulse.Internal.Def (OperationState(..))
import Sound.Pulse.Internal.Operation
import Sound.Pulse.Internal.MainLoopApi
import Sound.Pulse.Internal.MainLoop

-- | Meta data for operation
data Operation = Operation
    { opRaw :: RawOperationPtr
    , opMon :: TVar OperationState
    }

-- | The commands sent to the main loop.
data LoopCtl = Pause (TMVar (), TMVar ())

-- | The state of the loop.
data MainLoop = MainLoop
    { mlRaw :: RawMainLoopPtr
    , mlRunning :: TVar Bool -- ^ Token for the loop.
    , mlDead :: TVar Bool -- ^ Token for the loop.
    , mlCtl :: TChan LoopCtl -- ^ The channel the loop listens.
    , mlLock :: TMVar (ThreadId, Int) -- ^ Recursive lock for clients.
    , mlOps :: TVar [Operation] -- ^ Pending operations.
    }

getApi :: MainLoop -> IO MainLoopApiPtr
getApi = mainloopGetApi . mlRaw

-- | Allocate the resource and run!
newLoop :: IO MainLoop
newLoop = do
    raw <- mainloopNew
    running <- newTVarIO False
    dead <- newTVarIO False
    lock <- newEmptyTMVarIO
    ctl <- newTChanIO
    ops <- newTVarIO []
    -- return the loop
    return MainLoop
        { mlRaw = raw
        , mlRunning = running
        , mlDead = dead
        , mlCtl = ctl
        , mlLock = lock
        , mlOps = ops
        }

-- | Run the loop. Might throw 'LoopRunning' or 'LoopDead'.
runLoop :: MainLoop -> IO ()
runLoop ml = mask_ $ do
    atomically $ do
        void . check . not =<< readTVar (mlRunning ml)
        void . check . not =<< readTVar (mlDead ml)
        writeTVar (mlRunning ml) True
    -- fork and run the real loop
    void $ forkIO $ fix $ \loop -> do
        (res, _) <- mainloopIterate (mlRaw ml) True 0
        updateMonitors
        if res < 0
            then atomically $ writeTVar (mlRunning ml) False
            else handleCtl >> loop
    where
        updateMonitors :: IO ()
        updateMonitors = do
            ops <- readTVarIO (mlOps ml)
            maybeOps <- forM ops $ \op -> do
                state <- operationGetState (opRaw op)
                atomically $ writeTVar (opMon op) state
                case state of
                    OperationRunning ->
                        return $ Just op
                    OperationDone -> do
                        operationUnref (opRaw op)
                        return Nothing
                    OperationCancelled -> do
                        operationUnref (opRaw op)
                        return Nothing
            atomically $ writeTVar (mlOps ml) (catMaybes maybeOps)

        handleCtl :: IO ()
        handleCtl = do
            req <- atomically $ tryPeekTChan (mlCtl ml)
            case req of
                Nothing -> return ()
                Just (Pause (auth, reply)) -> do
                    atomically $ putTMVar auth ()
                    atomically $ readTMVar reply

quitLoop :: MainLoop -> IO ()
quitLoop ml = mainloopQuit (mlRaw ml) 0

-- | Stop the loop and free the resource.
freeLoop :: MainLoop -> IO ()
freeLoop ml = mask_ $ do
    atomically $ do
        void . check . not =<< readTVar (mlDead ml)
        void . check . not =<< readTVar (mlRunning ml)
        void . check . null =<< readTVar (mlOps ml)
        writeTVar (mlDead ml) True
    mainloopFree (mlRaw ml)

-------------------------------------------------------------
-- Blocking
-------------------------------------------------------------

type RecursiveLock = TMVar (ThreadId, Int)

-- | Acuire the lock, pause the loop
--   and run the client code.
--
--   Note that this does not gurantee fairness
--   among clients. We only gave the loop
--   some priority so that a loop will iterate
--   at least once after each client code.
blockLoop :: MainLoop -> IO a -> IO a
blockLoop ml code = bracket
    (atomically . acquireLock (mlLock ml) =<< myThreadId)
    (const $ atomically $ releaseLock (mlLock ml))
    (\needPause -> if needPause
        then bracket pauseLoop restartLoop (const code)
        else code)
    where
        pauseLoop :: IO (TMVar ())
        pauseLoop = do
            atomically $ void . check =<< readTVar (mlRunning ml)
            auth <- newEmptyTMVarIO
            reply <- newEmptyTMVarIO
            atomically $ writeTChan (mlCtl ml) (Pause (auth, reply))
            mainloopWakeup (mlRaw ml)
            atomically $ takeTMVar auth
            return reply

        restartLoop :: TMVar () -> IO ()
        restartLoop reply = atomically $ putTMVar reply ()

        acquireLock :: RecursiveLock -> ThreadId -> STM Bool
        acquireLock lock thread = do
            state <- tryTakeTMVar lock
            case state of
                Nothing -> do
                    putTMVar lock (thread, 1)
                    return True
                Just (t, n) -> if t == thread
                    then do
                        putTMVar lock (t, n + 1)
                        return False
                    else retry

        releaseLock :: RecursiveLock -> STM ()
        releaseLock lock = do
            state <- takeTMVar lock
            case state of
                (_, 1) -> return ()
                (t, n) -> putTMVar lock (t, n - 1)

-------------------------------------------------------------
-- Operations
-------------------------------------------------------------

-- | Wrap a raw operation and get a high-level monitor.
--   Monitors will be updated for each iteration. Note
--   that the loop will unref (decrease the reference count of)
--   the resource once the operation is done or cancelled.
--   The caller needs to increase the reference count if
--   it really wants to keep a copy of the raw pointer.
--   (Why not just use the high-level monitor, eh?)
wrapRawOp :: MainLoop -> RawOperationPtr -> IO (TVar OperationState)
wrapRawOp ml raw = mask_ $ do
    mon <- newTVarIO OperationRunning
    atomically $ modifyTVar' (mlOps ml) (Operation raw mon:)
    return mon
