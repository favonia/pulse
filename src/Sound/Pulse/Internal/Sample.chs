{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

{- |
This module provides the bindings to @sample.h@.
-}
module Sound.Pulse.Internal.Sample where

import Foreign

#include <pulse/sample.h>

{#enum sample_format as SampleFormat {underscoreToCase} deriving (Show, Eq) #}

data SampleSpec
{#pointer *sample_spec as SampleSpecPtr -> SampleSpec #}
