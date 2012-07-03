{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{- |
Module      :  Sound.Pulse.Monad
Copyright   :  (c) MnO2
License     :  BSD3

Maintainer  :  mno2.csie@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides the monadic interface.
-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.ChannelMap where

import Foreign

#include <pulse/channelmap.h>

{#enum channel_position as ChannelPosition {underscoreToCase} deriving (Show, Eq) #}

{#enum channel_map_def as ChannelMapDef {underscoreToCase} deriving (Show, Eq) #}

data ChannelMap = ChannelMap [ChannelPosition]
{#pointer *channel_map as ChannelMapPtr -> ChannelMap #}
