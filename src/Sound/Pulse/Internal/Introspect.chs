{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Introspect where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Volume #}
{#import Sound.Pulse.Internal.Context #}
{#import Sound.Pulse.Internal.Operation #}

#include <pulse/introspect.h>

data RawSinkPortInfo
{#pointer *sink_port_info as RawSinkPortInfoPtr -> RawSinkPortInfo #}

data RawSinkInfo
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
