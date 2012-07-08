{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{- |
Module      :  Sound.Pulse.Monad.Internal.Connection
Copyright   :  (c) MnO2
License     :  BSD3

Maintainer  :  mno2.csie@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

High-level interface for context.
-}

module Sound.Pulse.Monad.Internal.Introspect where

import Control.Monad hiding (forM_)
import Control.Concurrent.STM
import Control.Exception
import Data.Foldable (forM_)
import Data.Typeable
#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe hiding (void)
#else
import Foreign hiding (void)
#endif

import Sound.Pulse.Internal.C2HS
import Sound.Pulse.Internal.Context
import Sound.Pulse.Internal.Operation
import Sound.Pulse.Internal.Introspect

import Sound.Pulse.Monad.Internal.MainLoop
import Sound.Pulse.Monad.Internal.Connection


-- | Callback for getting sink info
sinkInfoCallback :: RawSinkInfoCallback a
sinkInfoCallback rawCtxPtr rawSinkInfoPtr eol monPtr'
    | eol >= 0 = 
        forM_ (castPtrToMaybeStable monPtr') $ \monPtr -> do
            mon <- deRefStablePtr monPtr
            rawSinkInfoList  <- readTVarIO mon
            currRawSinkInfo <- peek rawSinkInfoPtr
            atomically $ writeTVar mon (currRawSinkInfo:rawSinkInfoList)
    | otherwise = 
        -- error, do nothing
        return ()

-- | Ugly trick to create a static C wrapper.
foreign export ccall "_pulse_private_sinkInfoCallback"
    sinkInfoCallback :: RawSinkInfoCallback a
foreign import ccall "&_pulse_private_sinkInfoCallback"
    wrappedSinkInfoCallback :: FunPtr (RawSinkInfoCallback a)

getSinkInfo :: Context -> IO [RawSinkInfo]
getSinkInfo ctx = mask_ $ bracketOnError
    (do
        mon <- newTVarIO []
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let loop = ctxLoop ctx
            rawCtxPtr = ctxRaw ctx
        rawOpPtr <- contextGetSinkInfoList rawCtxPtr wrappedSinkInfoCallback (Just monPtr)
        monOpState <- wrapRawOp loop rawOpPtr
        atomically $ do
            state <- readTVar monOpState
            case state of 
               OperationRunning -> retry
               OperationDone -> return ()
               OperationCancelled -> throwSTM OperationFail
        rawSinkInfoList <- readTVarIO mon 
        return rawSinkInfoList


-- | Callback for getting source info
sourceInfoCallback :: RawSourceInfoCallback a
sourceInfoCallback rawCtxPtr rawSourceInfoPtr eol monPtr'
    | eol >= 0 = 
        forM_ (castPtrToMaybeStable monPtr') $ \monPtr -> do
            mon <- deRefStablePtr monPtr
            rawSourceInfoList  <- readTVarIO mon
            currRawSourceInfo <- peek rawSourceInfoPtr
            atomically $ writeTVar mon (currRawSourceInfo:rawSourceInfoList)
    | otherwise = 
        -- error, do nothing
        return ()

-- | Ugly trick to create a static C wrapper.
foreign export ccall "_pulse_private_sourceInfoCallback"
    sourceInfoCallback :: RawSourceInfoCallback a
foreign import ccall "&_pulse_private_sourceInfoCallback"
    wrappedSourceInfoCallback :: FunPtr (RawSourceInfoCallback a)


getSourceInfo :: Context -> IO [RawSourceInfo]
getSourceInfo ctx = mask_ $ bracketOnError 
    (do
        mon <- newTVarIO []
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let loop = ctxLoop ctx
            rawCtxPtr = ctxRaw ctx
        rawOpPtr <- contextGetSourceInfoList rawCtxPtr wrappedSourceInfoCallback (Just monPtr)
        monOpState <- wrapRawOp loop rawOpPtr
        atomically $ do
            state <- readTVar monOpState
            case state of 
               OperationRunning -> retry
               OperationDone -> return ()
               OperationCancelled -> throwSTM OperationFail
        rawSourceInfoList <- readTVarIO mon 
        return rawSourceInfoList


-- | Callback for getting source info
serverInfoCallback :: RawServerInfoCallback a
serverInfoCallback rawCtxPtr rawServerInfoPtr monPtr' = 
    forM_ (castPtrToMaybeStable monPtr') $ \monPtr -> do
        mon <- deRefStablePtr monPtr
        currRawServerInfo <- peek rawServerInfoPtr
        atomically $ writeTVar mon currRawServerInfo

-- | Ugly trick to create a static C wrapper.
foreign export ccall "_pulse_private_serverInfoCallback"
    serverInfoCallback :: RawServerInfoCallback a
foreign import ccall "&_pulse_private_serverInfoCallback"
    wrappedServerInfoCallback :: FunPtr (RawServerInfoCallback a)

getServerInfo :: Context -> IO RawServerInfo
getServerInfo ctx = mask_ $ bracketOnError 
    (do
        mon <- newTVarIO $ RawServerInfo Nothing Nothing Nothing Nothing Nothing Nothing
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let loop = ctxLoop ctx
            rawCtxPtr = ctxRaw ctx
        rawOpPtr <- contextGetServerInfo rawCtxPtr wrappedServerInfoCallback (Just monPtr)
        monOpState <- wrapRawOp loop rawOpPtr
        atomically $ do
            state <- readTVar monOpState
            case state of 
               OperationRunning -> retry
               OperationDone -> return ()
               OperationCancelled -> throwSTM OperationFail
        rawServerInfo <- readTVarIO mon 
        return rawServerInfo

data OperationFail = OperationFail deriving (Show, Typeable)

instance Exception OperationFail where
