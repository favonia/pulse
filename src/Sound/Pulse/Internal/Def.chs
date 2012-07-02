{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Def where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Control.Monad (liftM)
import Control.Applicative
import Sound.Pulse.Internal.C2HS

#include <pulse/def.h>

{#enum context_state as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum stream_state as ^ {underscoreToCase} deriving (Show, Eq) #}

-- | The state of the operation.
{#enum operation_state as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum context_flags as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum device_type as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum stream_direction as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum stream_flags as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum subscription_mask as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum subscription_event_type_t as ^ {underscoreToCase} deriving (Show, Eq) #}

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


{#enum seek_mode as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum sink_flags as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum sink_state as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum source_flags as ^ {underscoreToCase} deriving (Show, Eq) #}

{#enum source_state as ^ {underscoreToCase} deriving (Show, Eq) #}
