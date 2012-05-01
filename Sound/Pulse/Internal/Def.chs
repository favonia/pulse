{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Def where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Data.Word (Word32)
import Control.Monad (liftM)
import Control.Applicative
import Sound.Pulse.Internal.C2HS

#include <pulse/def.h>

{#enum pa_context_state as ContextState {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_stream_state as StreamState {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_operation_state as OperationState {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_context_flags as ContextFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_device_type as DeviceType {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_stream_direction as StreamDirection {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_stream_flags as StreamFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_subscription_mask as SubscriptionMask {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_subscription_event_type_t as SubscriptionEventType {underscoreToCase} deriving (Show, Eq) #}

data TimingInfo
{#pointer *pa_timing_info as TimingInfoPtr -> TimingInfo #}

data BufferAttr = BufferAttr { maxlength :: Word32, 
                               tlength :: Word32, 
                               prebuf :: Word32, 
                               minreq :: Word32, 
                               fragsize :: Word32}

{#pointer *pa_buffer_attr as BufferAttrPtr -> BufferAttr #}

instance Storable BufferAttr where
    sizeOf _ = {#sizeof pa_buffer_attr #}
    alignment _ = 4
    peek p = BufferAttr 
        <$> liftM cIntConv ({#get pa_buffer_attr->maxlength #} p)
        <*> liftM cIntConv ({#get pa_buffer_attr->tlength #} p)
        <*> liftM cIntConv ({#get pa_buffer_attr->prebuf #} p)
        <*> liftM cIntConv ({#get pa_buffer_attr->minreq #} p)
        <*> liftM cIntConv ({#get pa_buffer_attr->fragsize #} p)
    poke p x = do
        {#set pa_buffer_attr.maxlength #} p (cIntConv $ maxlength x)
        {#set pa_buffer_attr.tlength #} p (cIntConv $ tlength x)
        {#set pa_buffer_attr.prebuf #} p (cIntConv $ prebuf x)
        {#set pa_buffer_attr.minreq #} p (cIntConv $ minreq x)
        {#set pa_buffer_attr.fragsize #} p (cIntConv $ fragsize x)
        
data SpawnApi
{#pointer *pa_spawn_api as SpawnApiPtr -> SpawnApi #}


{#enum pa_seek_mode as SeekMode {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_sink_flags as SinkFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_sink_state as SinkState {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_source_flags as SourceFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_source_state as SourceState {underscoreToCase} deriving (Show, Eq) #}
