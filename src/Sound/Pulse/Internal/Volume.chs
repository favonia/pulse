{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
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

