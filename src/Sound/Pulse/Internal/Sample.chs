{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

{- |
This module provides the bindings to @sample.h@.
-}
module Sound.Pulse.Internal.Sample where

import Foreign.Safe
import Foreign.C
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Sound.Pulse.Internal.C2HS

#include <pulse/sample.h>

{#enum sample_format as SampleFormat {underscoreToCase} deriving (Show, Eq) #}

data SampleSpec = SampleSpec
    { format'SampleSpec :: SampleFormat
    , rate'SampleSpec :: Word
    , channels'SampleSpec :: Word
    }

instance Storable SampleSpec where
    sizeOf _ = {#sizeof pa_sample_spec #}
    alignment _ = {#alignof pa_sample_spec #}
    peek p = SampleSpec
                <$> (liftM cToEnum ({#get pa_sample_spec->format #} p))
                <*> (liftM cIntConv ({#get pa_sample_spec->rate #} p))
                <*> (liftM cIntConv ({#get pa_sample_spec->channels #} p))
    poke p x = return ()

{#pointer *sample_spec as SampleSpecPtr -> SampleSpec #}
