{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Def where

import Foreign.Safe
import Foreign.C
import Control.Monad (liftM)
import Control.Applicative
import Sound.Pulse.Internal.C2HS

#include <pulse/def.h>

{#enum context_state as ContextState {underscoreToCase} deriving (Show, Eq) #}

{#enum stream_state as StreamState {underscoreToCase} deriving (Show, Eq) #}

{#enum operation_state as OperationState {underscoreToCase} deriving (Show, Eq) #}

{#enum context_flags as ContextFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum device_type as DeviceType {underscoreToCase} deriving (Show, Eq) #}

{#enum stream_direction as StreamDirection {underscoreToCase} deriving (Show, Eq) #}

{#enum stream_flags as StreamFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum subscription_mask as SubscriptionMask {underscoreToCase} deriving (Show, Eq) #}

{#enum subscription_event_type_t as SubscriptionEventType {underscoreToCase} deriving (Show, Eq) #}

data TimingInfo
{#pointer *timing_info as TimingInfoPtr -> TimingInfo #}

data BufferAttr = BufferAttr { maxlength :: Word32,
                               tlength :: Word32,
                               prebuf :: Word32,
                               minreq :: Word32,
                               fragsize :: Word32}

{#pointer *buffer_attr as BufferAttrPtr -> BufferAttr #}

instance Storable BufferAttr where
    sizeOf _ = {#sizeof buffer_attr #}
    alignment _ = 4
    peek p = BufferAttr
        <$> liftM cIntConv ({#get buffer_attr->maxlength #} p)
        <*> liftM cIntConv ({#get buffer_attr->tlength #} p)
        <*> liftM cIntConv ({#get buffer_attr->prebuf #} p)
        <*> liftM cIntConv ({#get buffer_attr->minreq #} p)
        <*> liftM cIntConv ({#get buffer_attr->fragsize #} p)
    poke p x = do
        {#set buffer_attr.maxlength #} p (cIntConv $ maxlength x)
        {#set buffer_attr.tlength #} p (cIntConv $ tlength x)
        {#set buffer_attr.prebuf #} p (cIntConv $ prebuf x)
        {#set buffer_attr.minreq #} p (cIntConv $ minreq x)
        {#set buffer_attr.fragsize #} p (cIntConv $ fragsize x)

data SpawnApi
{#pointer *spawn_api as SpawnApiPtr -> SpawnApi #}


{#enum seek_mode as SeekMode {underscoreToCase} deriving (Show, Eq) #}

{#enum sink_flags as SinkFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum sink_state as SinkState {underscoreToCase} deriving (Show, Eq) #}

{#enum source_flags as SourceFlags {underscoreToCase} deriving (Show, Eq) #}

{#enum source_state as SourceState {underscoreToCase} deriving (Show, Eq) #}
