{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.ChannelMap where

import Foreign

#include <pulse/channelmap.h>

{#enum channel_position as ChannelPosition {underscoreToCase} deriving (Show, Eq) #}

{#enum channel_map_def as ChannelMapDef {underscoreToCase} deriving (Show, Eq) #}

data ChannelMap = ChannelMap [ChannelPosition] 
{#pointer *channel_map as ChannelMapPtr -> ChannelMap #}
