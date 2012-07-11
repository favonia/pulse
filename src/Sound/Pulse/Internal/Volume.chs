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
This module provides the bindings to @volume.h@.
-}
module Sound.Pulse.Internal.Volume where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C

import Control.Monad (liftM)

import Sound.Pulse.Internal.C2HS

#include <pulse/volume.h>

type Volume = Integer

data RawCVolume = CVolume
    { channeVolumes :: [Volume]
    , volRaw :: RawCVolumePtr
    }

instance Storable RawCVolume where
    sizeOf _ = {#sizeof pa_cvolume #}
    alignment _ = {#alignof pa_cvolume #}
    peek p = do
        channelNum <- liftM cIntConv ({#get pa_cvolume->channels #} p)
        rawArrayHead <- ({#get pa_cvolume->values #} p)
        {- channelVolumes <- liftM cIntConv $ peekArray channelNum rawArrayHead -}
        return $ CVolume undefined p
    poke p (CVolume vol raw) = do
        {#set pa_cvolume.channels #} p undefined
        {#set pa_cvolume.values #} p undefined

{#pointer *pa_cvolume as RawCVolumePtr -> RawCVolume #}
