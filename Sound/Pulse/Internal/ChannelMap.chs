{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Sound.Pulse.Internal.ChannelMap where

import Foreign.Storable
import Foreign.Ptr

#include <pulse/channelmap.h>

{#enum pa_channel_position as ChannelPosition {underscoreToCase} deriving (Show, Eq) #}

{#enum pa_channel_map_def as ChannelMapDef {underscoreToCase} deriving (Show, Eq) #}

data ChannelMap = ChannelMap [ChannelPosition] 
{#pointer *pa_channel_map as ChannelMapPtr -> ChannelMap #}

