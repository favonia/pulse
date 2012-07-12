{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

{- |
This module provides the bindings to @introspect.h@.
-}
module Sound.Pulse.Internal.Introspect where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.Word (Word64)
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Def #}
{#import Sound.Pulse.Internal.Volume #}
{#import Sound.Pulse.Internal.Context #}
{#import Sound.Pulse.Internal.Operation #}
{#import Sound.Pulse.Internal.Format #}
{#import Sound.Pulse.Internal.ChannelMap #}

#include <pulse/introspect.h>

type MuSecond = Word64

data RawSinkPortInfo = RawSinkPortInfo
    { name'RawSinkPortInfo :: Maybe String
    , description'RawSinkPortInfo :: Maybe String
    , priority'RawSinkPortInfo :: Int
    }

instance Storable RawSinkPortInfo where
    sizeOf _ = {#sizeof pa_sink_port_info #}
    alignment _ = {#alignof pa_sink_port_info #}
    peek p = RawSinkPortInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_sink_port_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_sink_port_info->description #} p))
        <*> (liftM cIntConv ({#get pa_sink_port_info->priority #} p))
    poke p x = do
        {#set pa_sink_port_info.name #} p undefined
        {#set pa_sink_port_info.description #} p undefined
        {#set pa_sink_port_info.priority #} p undefined
{#pointer *sink_port_info as RawSinkPortInfoPtr -> RawSinkPortInfo #}

data RawSinkInfo = RawSinkInfo
    { sinkName'RawSinkInfo :: Maybe String
    , sinkIndex'RawSinkInfo :: Int
    , sinkDesc'RawSinkInfo :: Maybe String
  {-  , sinkChannelMap'RawSinkInfo :: ChannelMap -}
    , sinkMute'RawSinkInfo :: Bool
    , sinkLatency'RawSinkInfo :: MuSecond
    , sinkFlags'RawSinkInfo :: SinkFlags
    , sinkConfiguredLatency'RawSinkInfo :: MuSecond
    , sinkBaseVolume'RawSinkInfo :: Volume
    , sinkState'RawSinkInfo :: SinkState
    }

instance Storable RawSinkInfo where
    sizeOf _ = {#sizeof pa_sink_info #}
    alignment _ = {#alignof pa_sink_info #}
    peek p = RawSinkInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_sink_info->name #} p))
        <*> (liftM cIntConv ({#get pa_sink_info->index #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_sink_info->description #} p))
        {-<*> (peekNullableUTF8CString =<< ({#get pa_sink_info->channel_map #} p)) -}
        <*> (liftM cToBool ({#get pa_sink_info->mute #} p))
        <*> (liftM cIntConv ({#get pa_sink_info->latency #} p))
        <*> (liftM cToEnum ({#get pa_sink_info->flags #} p))
        <*> (liftM cIntConv ({#get pa_sink_info->configured_latency #} p))
        <*> (liftM cIntConv ({#get pa_sink_info->base_volume #} p))
        <*> (liftM cToEnum ({#get pa_sink_info->state #} p))
    poke p x = do
        {#set pa_sink_info.name #} p undefined
        {#set pa_sink_info.index #} p undefined
        {#set pa_sink_info.description #} p undefined

{#pointer *sink_info as RawSinkInfoPtr -> RawSinkInfo #}

type RawSinkInfoCallback a = RawContextPtr -> RawSinkInfoPtr -> CInt -> RawUserData a -> IO ()


{#fun context_get_sink_info_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      id `FunPtr (RawSinkInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}


{#fun context_get_sink_info_by_index as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawSinkInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}


{#fun context_get_sink_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawSinkInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_set_sink_volume_by_index as ^
    { id `RawContextPtr',
      `Int',
      id `RawCVolumePtr',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_sink_volume_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      id `RawCVolumePtr',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_sink_mute_by_index as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_sink_mute_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_suspend_sink_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_suspend_sink_by_index as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_sink_port_by_index as ^
    { id `RawContextPtr',
      `Int',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_sink_port_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


data RawSourcePortInfo = RawSourcePortInfo
    { name'RawSourcePortInfo :: Maybe String
    , description'RawSourcePortInfo :: Maybe String
    , priority'RawSourcePortInfo :: Int
    }

instance Storable RawSourcePortInfo where
    sizeOf _ = {#sizeof pa_source_port_info #}
    alignment _ = {#alignof pa_source_port_info #}
    peek p = RawSourcePortInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_source_port_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_source_port_info->description #} p))
        <*> (liftM cIntConv ({#get pa_source_port_info->priority #} p))
    poke p x = do
        {#set pa_source_port_info.name #} p undefined
        {#set pa_source_port_info.description #} p undefined
        {#set pa_source_port_info.priority #} p undefined

{#pointer *source_port_info as RawSourcePortInfoPtr -> RawSourcePortInfo #}

data RawSourceInfo = RawSourceInfo
    { sourceName'RawSourceInfo :: Maybe String
    , sourceDesc'RawSourceInfo :: Maybe String
    }

instance Storable RawSourceInfo where
    sizeOf _ = {#sizeof pa_source_info #}
    alignment _ = {#alignof pa_source_info #}
    peek p = RawSourceInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_source_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_source_info->description #} p))
    poke p x = do
        {#set pa_source_info.name #} p undefined
        {#set pa_source_info.description #} p undefined
{#pointer *source_info as RawSourceInfoPtr -> RawSourceInfo #}


type RawSourceInfoCallback a = RawContextPtr -> RawSourceInfoPtr -> CInt -> RawUserData a -> IO ()

{#fun context_get_source_info_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      id `FunPtr (RawSourceInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}


{#fun context_get_source_info_by_index as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawSourceInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_get_source_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawSourceInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_set_source_volume_by_index as ^
    { id `RawContextPtr',
      `Int',
      id `RawCVolumePtr',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_source_volume_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      id `RawCVolumePtr',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_source_mute_by_index as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_source_mute_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_suspend_source_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_suspend_source_by_index as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_source_port_by_index as ^
    { id `RawContextPtr',
      `Int',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_source_port_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}



data RawServerInfo = RawServerInfo
    { userName'RawServerInfo :: Maybe String
    , hostName'RawServerInfo :: Maybe String
    , serverVersion'RawServerInfo :: Maybe String
    , serverName'RawServerInfo :: Maybe String
    , defaultSinkName'RawServerInfo :: Maybe String
    , defaultSourceName'RawServerInfo :: Maybe String
    }
{#pointer *server_info as RawServerInfoPtr -> RawServerInfo #}

instance Storable RawServerInfo where
    sizeOf _ = {#sizeof pa_server_info #}
    alignment _ = {#alignof pa_server_info #}
    peek p = RawServerInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_server_info->user_name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_server_info->host_name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_server_info->server_version #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_server_info->server_name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_server_info->default_sink_name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_server_info->default_source_name #} p))
    poke p x = do
        {#set pa_server_info.user_name #} p undefined
        {#set pa_server_info.host_name #} p undefined
        {#set pa_server_info.server_version #} p undefined
        {#set pa_server_info.server_name #} p undefined
        {#set pa_server_info.default_sink_name #} p undefined
        {#set pa_server_info.default_source_name #} p undefined

type RawServerInfoCallback a = RawContextPtr -> RawServerInfoPtr -> RawUserData a -> IO ()


{#fun context_get_server_info as ^
    { id `RawContextPtr',
      id `FunPtr (RawServerInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}



data RawModuleInfo = RawModuleInfo
    { moduleName'RawModuleInfo :: Maybe String
    }

instance Storable RawModuleInfo where
    sizeOf _ = {#sizeof pa_module_info #}
    alignment _ = {#alignof pa_module_info #}
    peek p = RawModuleInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_module_info->name #} p))
    poke p x = do
        {#set pa_module_info.name #} p undefined

{#pointer *module_info as RawModuleInfoPtr -> RawModuleInfo #}


type RawModuleInfoCallback a = RawContextPtr -> RawModuleInfoPtr -> CInt -> RawUserData a -> IO ()


{#fun context_get_module_info as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawModuleInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_get_module_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawModuleInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


type RawContextIndexCallback a = RawContextPtr -> CUInt -> RawUserData a -> IO ()


{#fun context_load_module as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      withUTF8CString* `String',
      id `FunPtr (RawContextIndexCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


{#fun context_unload_module as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


data RawClientInfo = RawClientInfo
    { clientName'RawClientInfo :: Maybe String
    , driverName'RawClientInfo :: Maybe String
    }

instance Storable RawClientInfo where
    sizeOf _ = {#sizeof pa_client_info #}
    alignment _ = {#alignof pa_client_info #}
    peek p = RawClientInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_client_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_client_info->driver #} p))
    poke p x = do
        {#set pa_client_info.name #} p undefined
        {#set pa_client_info.driver #} p undefined
{#pointer *client_info as RawClientInfoPtr -> RawClientInfo #}


type RawClientInfoCallback a = RawContextPtr -> RawClientInfoPtr -> CInt -> RawUserData a -> IO ()


{#fun context_get_client_info as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawClientInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


{#fun context_get_client_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawClientInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_kill_client as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


data RawCardProfileInfo
{#pointer *card_profile_info as RawCardProfileInfoPtr -> RawCardProfileInfo #}

#if PA_CHECK_VERSION(2,0,0)
data RawCardPortInfo
{#pointer *card_port_info as RawCardPortInfoPtr -> RawCardPortInfo #}
#endif

data RawCardInfo = RawCardInfo
    { cardName'RawCardInfo :: Maybe String
    , driverName'RawCardInfo :: Maybe String
    }

instance Storable RawCardInfo where
    sizeOf _ = {#sizeof pa_card_info #}
    alignment _ = {#alignof pa_card_info #}
    peek p = RawCardInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_card_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_card_info->driver #} p))
    poke p x = do
        {#set pa_card_info.name #} p undefined
        {#set pa_card_info.driver #} p undefined
{#pointer *card_info as RawCardInfoPtr -> RawCardInfo #}


type RawCardInfoCallback a = RawContextPtr -> RawCardInfoPtr -> CInt -> RawUserData a -> IO ()



{#fun context_get_card_info_by_index as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawCardInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_get_card_info_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      id `FunPtr (RawCardInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_get_card_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawCardInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


{#fun context_set_card_profile_by_index as ^
    { id `RawContextPtr',
      `Int',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


{#fun context_set_card_profile_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


data RawSinkInputInfo = RawSinkInputInfo
    { sinkInputName'RawSinkInputInfo :: Maybe String
    , driverName'RawSinkInputInfo :: Maybe String
    }

instance Storable RawSinkInputInfo where
    sizeOf _ = {#sizeof pa_sink_input_info #}
    alignment _ = {#alignof pa_sink_input_info #}
    peek p = RawSinkInputInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_sink_input_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_sink_input_info->driver #} p))
    poke p x = do
        {#set pa_sink_input_info.name #} p undefined
        {#set pa_sink_input_info.driver #} p undefined
{#pointer *sink_input_info as RawSinkInputInfoPtr -> RawSinkInputInfo #}


type RawSinkInputInfoCallback a = RawContextPtr -> RawSinkInputInfoPtr -> CInt -> RawUserData a -> IO ()


{#fun context_get_sink_input_info as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawSinkInputInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_get_sink_input_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawSinkInputInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_move_sink_input_by_name as ^
    { id `RawContextPtr',
      `Int',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_move_sink_input_by_index as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_sink_input_volume as ^
    { id `RawContextPtr',
      `Int',
      id `RawCVolumePtr',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_sink_input_mute as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_kill_sink_input as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}



data RawSourceOutputInfo = RawSourceOutputInfo
    { sourceOutputName'RawSourceOutputInfo :: Maybe String
    , driverName'RawSourceOutputInfo :: Maybe String
    }

instance Storable RawSourceOutputInfo where
    sizeOf _ = {#sizeof pa_source_output_info #}
    alignment _ = {#alignof pa_source_output_info #}
    peek p = RawSourceOutputInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_source_output_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_source_output_info->driver #} p))
    poke p x = do
        {#set pa_source_output_info.name #} p undefined
        {#set pa_source_output_info.driver #} p undefined
{#pointer *source_output_info as RawSourceOutputInfoPtr -> RawSourceOutputInfo #}


type RawSourceOutputInfoCallback a = RawContextPtr -> RawSourceOutputInfoPtr -> CInt -> RawUserData a -> IO ()



{#fun context_get_source_output_info as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawSourceOutputInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_get_source_output_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawSourceOutputInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_move_source_output_by_name as ^
    { id `RawContextPtr',
      `Int',
      withUTF8CString* `String',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_move_source_output_by_index as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_source_output_volume as ^
    { id `RawContextPtr',
      `Int',
      id `RawCVolumePtr',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_set_source_output_mute as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_kill_source_output as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}



data RawStatInfo = RawStatInfo
    { memblockTotal'RawStatInfo :: Int
    , memblockTotalSize'RawStatInfo :: Int
    }

instance Storable RawStatInfo where
    sizeOf _ = {#sizeof pa_stat_info #}
    alignment _ = {#alignof pa_stat_info #}
    peek p = RawStatInfo
        <$> liftM cIntConv ({#get pa_stat_info->memblock_total #} p)
        <*> liftM cIntConv ({#get pa_stat_info->memblock_total_size #} p)
    poke p x = do
        {#set pa_stat_info.memblock_total #} p undefined
        {#set pa_stat_info.memblock_total_size #} p undefined
{#pointer *stat_info as RawStatInfoPtr -> RawStatInfo #}

type RawStatInfoCallback a = RawContextPtr -> RawStatInfoPtr -> RawUserData a -> IO ()


{#fun context_stat as ^
    { id `RawContextPtr',
      id `FunPtr (RawStatInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}


data RawSampleInfo = RawSampleInfo
    { sampleName'RawSampleInfo :: Maybe String
    , fileName'RawSampleInfo :: Maybe String
    }

instance Storable RawSampleInfo where
    sizeOf _ = {#sizeof pa_sample_info #}
    alignment _ = {#alignof pa_sample_info #}
    peek p = RawSampleInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_sample_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_sample_info->filename #} p))
    poke p x = do
        {#set pa_sample_info.name #} p undefined
        {#set pa_sample_info.filename #} p undefined
{#pointer *sample_info as RawSampleInfoPtr -> RawSampleInfo #}

type RawSampleInfoCallback a = RawContextPtr -> RawSampleInfoPtr -> CInt -> RawUserData a -> IO ()


{#fun context_get_sample_info_by_name as ^
    { id `RawContextPtr',
      withUTF8CString* `String',
      id `FunPtr (RawSampleInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_get_sample_info_by_index as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawSampleInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

{#fun context_get_sample_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawSampleInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}

