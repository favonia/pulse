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
import Sound.Pulse.Internal.Volume

import Sound.Pulse.Monad.Internal.Connection


-- | Callback for getting sink info
sinkInfoCallback :: RawSinkInfoCallback a
sinkInfoCallback rawCtxPtr rawSinkInfoPtr eol monPtr'
    | eol == 0 =
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
getSinkInfo ctx = mask_ $ bracket
    (do
        mon <- newTVarIO []
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let rawCtxPtr = ctxRaw ctx
        autoWait ctx =<< contextGetSinkInfoList rawCtxPtr wrappedSinkInfoCallback (Just monPtr)
        rawSinkInfoList <- readTVarIO mon
        return rawSinkInfoList


-- | Callback for getting sink info
contextSuccessCallback :: RawContextSuccessCallback a
contextSuccessCallback rawCtxPtr isSuccessful monPtr'
    | isSuccessful > 0 =
        forM_ (castPtrToMaybeStable monPtr') $ \monPtr -> do
            mon <- deRefStablePtr monPtr
            atomically $ writeTVar mon True
    | otherwise =
        forM_ (castPtrToMaybeStable monPtr') $ \monPtr -> do
            mon <- deRefStablePtr monPtr
            atomically $ writeTVar mon False

-- | Ugly trick to create a static C wrapper.
foreign export ccall "_pulse_private_contextSuccessCallback"
    contextSuccessCallback :: RawContextSuccessCallback a
foreign import ccall "&_pulse_private_contextSuccessCallback"
    wrappedContextSuccessCallback :: FunPtr (RawContextSuccessCallback a)


type VolumeSetterByIndex a = RawContextPtr -> Int -> RawCVolumePtr -> FunPtr (RawContextSuccessCallback a) -> UserData a -> IO (RawOperationPtr)
type VolumeSetterByName a = RawContextPtr -> String -> RawCVolumePtr -> FunPtr (RawContextSuccessCallback a) -> UserData a -> IO (RawOperationPtr)

setSinkVolume :: Context -> Either String Int -> RawCVolume -> IO Bool
setSinkVolume = setVolumeHelper contextSetSinkVolumeByName contextSetSinkVolumeByIndex

setSourceVolume :: Context -> Either String Int -> RawCVolume -> IO Bool
setSourceVolume = setVolumeHelper contextSetSourceVolumeByName contextSetSourceVolumeByIndex

setSinkInputVolume :: Context -> Int -> RawCVolume -> IO Bool
setSinkInputVolume ctx idx cvol = setVolumeHelper undefined contextSetSinkInputVolume ctx (Right idx) cvol

setSourceOutputVolume :: Context -> Int -> RawCVolume -> IO Bool
setSourceOutputVolume ctx idx cvol = setVolumeHelper undefined contextSetSourceOutputVolume ctx (Right idx) cvol

setVolumeHelper :: VolumeSetterByName (TVar Bool) -> VolumeSetterByIndex (TVar Bool) -> Context -> Either String Int -> RawCVolume -> IO Bool
setVolumeHelper setterByName setterByIndex ctx identifier cvol = mask_ $ bracket
    (do
        mon <- newTVarIO False
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let rawCtxPtr = ctxRaw ctx
            rawCVolPtr = volRaw cvol
        case identifier of
            Left name -> autoWait ctx =<< setterByName rawCtxPtr name rawCVolPtr wrappedContextSuccessCallback (Just monPtr)
            Right idx -> autoWait ctx =<< setterByIndex rawCtxPtr idx rawCVolPtr wrappedContextSuccessCallback (Just monPtr)
        isSuccessful <- readTVarIO mon
        return isSuccessful


type MuteSetterByIndex a = RawContextPtr -> Int -> Int -> FunPtr (RawContextSuccessCallback a) -> UserData a -> IO (RawOperationPtr)
type MuteSetterByName a = RawContextPtr -> String -> Int -> FunPtr (RawContextSuccessCallback a) -> UserData a -> IO (RawOperationPtr)


setSinkMute :: Context -> Either String Int -> Bool -> IO Bool
setSinkMute = setMuteHelper contextSetSinkMuteByName contextSetSinkMuteByIndex

setSourceMute :: Context -> Either String Int -> Bool -> IO Bool
setSourceMute = setMuteHelper contextSetSourceMuteByName contextSetSourceMuteByIndex

setSinkInputMute :: Context -> Int -> Bool -> IO Bool
setSinkInputMute ctx idx mute = setMuteHelper undefined contextSetSinkInputMute ctx (Right idx) mute

setSourceOutputMute :: Context -> Int -> Bool -> IO Bool
setSourceOutputMute ctx idx mute = setMuteHelper undefined contextSetSourceOutputMute ctx (Right idx) mute

setMuteHelper :: MuteSetterByName (TVar Bool) -> MuteSetterByIndex (TVar Bool) -> Context -> Either String Int -> Bool -> IO Bool
setMuteHelper setterByName setterByIndex ctx identifier mute = mask_ $ bracket
    (do
        mon <- newTVarIO False
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let rawCtxPtr = ctxRaw ctx
            muteInt = fromBool mute
        case identifier of
            Left name -> autoWait ctx =<< setterByName rawCtxPtr name muteInt wrappedContextSuccessCallback (Just monPtr)
            Right idx -> autoWait ctx =<< setterByIndex rawCtxPtr idx muteInt wrappedContextSuccessCallback (Just monPtr)
        isSuccessful <- readTVarIO mon
        return isSuccessful


-- | Callback for getting source info
sourceInfoCallback :: RawSourceInfoCallback a
sourceInfoCallback rawCtxPtr rawSourceInfoPtr eol monPtr'
    | eol == 0 =
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
getSourceInfo ctx = mask_ $ bracket
    (do
        mon <- newTVarIO []
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let rawCtxPtr = ctxRaw ctx
        autoWait ctx =<< contextGetSourceInfoList rawCtxPtr wrappedSourceInfoCallback (Just monPtr)
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
getServerInfo ctx = mask_ $ bracket
    (do
        mon <- newTVarIO $ RawServerInfo Nothing Nothing Nothing Nothing Nothing Nothing
        monPtr <- newStablePtr mon
        return (mon, monPtr))
    (freeStablePtr . snd)
    $ \(mon, monPtr) -> do
        let rawCtxPtr = ctxRaw ctx
        autoWait ctx =<< contextGetServerInfo rawCtxPtr wrappedServerInfoCallback (Just monPtr)
        rawServerInfo <- readTVarIO mon
        return rawServerInfo
