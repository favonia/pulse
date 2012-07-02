{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Volume where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C

#include <pulse/volume.h>

type Volume = Integer

data RawCVolume
{#pointer *pa_cvolume as RawCVolumePtr -> RawCVolume #}

