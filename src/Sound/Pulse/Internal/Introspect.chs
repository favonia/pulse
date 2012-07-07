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
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Volume #}
{#import Sound.Pulse.Internal.Context #}
{#import Sound.Pulse.Internal.Operation #}

#include <pulse/introspect.h>

data RawSinkPortInfo
{#pointer *sink_port_info as RawSinkPortInfoPtr -> RawSinkPortInfo #}

data RawSinkInfo = RawSinkInfo 
    { sinkName'RawSinkInfo :: Maybe String
    , sinkDesc'RawSinkInfo :: Maybe String
    }
   
instance Storable RawSinkInfo where
    sizeOf _ = {#sizeof pa_sink_info #}
    alignment _ = 4
    peek p = RawSinkInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_sink_info->name #} p))
        <*> (peekNullableUTF8CString =<< ({#get pa_sink_info->description #} p))
    poke p x = do
        {#set pa_sink_info.name #} p undefined
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
    

data RawSourcePortInfo
{#pointer *source_port_info as RawSourcePortInfoPtr -> RawSourcePortInfo #}

data RawSourceInfo
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
    


data RawServerInfo
{#pointer *server_info as RawServerInfoPtr -> RawServerInfo #}


type RawServerInfoCallback a = RawContextPtr -> RawServerInfoPtr -> RawUserData a -> IO ()


{#fun context_get_server_info as ^
    { id `RawContextPtr', 
      id `FunPtr (RawServerInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}
    


data RawModuleInfo
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
    

data RawClientInfo
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

data RawCardInfo
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
    

data RawSinkInputInfo
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
    

{#fun context_move_sink_input_by_index as ^
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
    


data RawSourceOutputInfo
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
    


data RawStatInfo
{#pointer *stat_info as RawStatInfoPtr -> RawStatInfo #}

type RawStatInfoCallback a = RawContextPtr -> RawStatInfoPtr -> RawUserData a -> IO ()


{#fun context_stat as ^
    { id `RawContextPtr', 
      id `FunPtr (RawStatInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr'  id #}
    

data RawSampleInfo
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
    
