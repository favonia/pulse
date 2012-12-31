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
This module provides the bindings to @timeval.h@.
-}
module Sound.Pulse.Internal.TimeVal where

import Foreign.Safe

#include <pulse/timeval.h>

-- Timeval
data RawTimeVal
{#pointer *timeval as RawTimeValPtr -> RawTimeVal #}

