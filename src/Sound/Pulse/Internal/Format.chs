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
This module provides the bindings to @format.h@.
-}
module Sound.Pulse.Internal.Format where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Control.Applicative ((<$>), (<*>))
import Sound.Pulse.Internal.C2HS

#include <pulse/format.h>

{#enum encoding as ^ {underscoreToCase} deriving (Show, Eq) #}

data RawFormatInfo = RawFormatInfo
    { encoding'RawFormatInfo :: Encoding
    }

{#pointer *format_info as RawFormatInfoPtr -> RawFormatInfo #}

