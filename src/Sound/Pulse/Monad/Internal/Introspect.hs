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
import Data.Foldable (forM_)
import Control.Concurrent.STM
import Control.Exception
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
getSinkInfo ctx = mask_ $ do
    let loop = ctxLoop ctx
        rawCtxPtr = ctxRaw ctx
    rawOpPtr <- contextGetSinkInfoList rawCtxPtr wrappedSinkInfoCallback Nothing
    bracketOnError
        (do
            mon <- newTVarIO []
            monPtr <- newStablePtr mon
            return (mon, monPtr))
        (freeStablePtr . snd)
        $ \(mon, monPtr) -> do
            bracketOnError
                (runLoop loop)
                (const $ quitLoop loop)
                $ const $ do
                    atomically $ do
                       rawSinkInfo  <- readTVar mon
                       return rawSinkInfo
