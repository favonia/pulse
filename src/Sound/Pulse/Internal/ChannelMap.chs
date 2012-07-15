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
This module provides the bindings to @channelmap.h@.
-}
module Sound.Pulse.Internal.ChannelMap where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Control.Monad (liftM)
import Sound.Pulse.Internal.C2HS

#include <pulse/channelmap.h>

{#enum channel_position as ChannelPosition {underscoreToCase} deriving (Show, Eq) #}

{#enum channel_map_def as ChannelMapDef {underscoreToCase} deriving (Show, Eq) #}

data ChannelMap = ChannelMap [ChannelPosition]

instance Storable ChannelMap where
    sizeOf _ = {#sizeof pa_channel_map #}
    alignment _ = {#alignof pa_channel_map #}
    peek p = do
        channelNum <- liftM cIntConv ({#get pa_channel_map->channels #} p)
        mapPtr <- ({#get pa_channel_map->map #} p)
        cPosList <- (peekArray channelNum mapPtr)
        let posList = fmap cToEnum cPosList
        return $ ChannelMap posList

    poke p x = return ()

{#pointer *channel_map as ChannelMapPtr -> ChannelMap #}
