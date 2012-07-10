{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

{- |
This module provides the bindings to @channelmap.h@.
-}
module Sound.Pulse.Internal.ChannelMap where

import Foreign

#include <pulse/channelmap.h>

{#enum channel_position as ChannelPosition {underscoreToCase} deriving (Show, Eq) #}

{#enum channel_map_def as ChannelMapDef {underscoreToCase} deriving (Show, Eq) #}

data ChannelMap = ChannelMap [ChannelPosition]
{#pointer *channel_map as ChannelMapPtr -> ChannelMap #}
