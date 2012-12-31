{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

{- |
This module provides the bindings to @version.h@.
-}
module Sound.Pulse.Internal.Version where

#include <pulse/version.h>

apiVersion :: Int
apiVersion = #const PA_API_VERSION

protocolVersion :: Int
protocolVersion = #const PA_PROTOCOL_VERSION

majorVersionNumber :: Int
majorVersionNumber = #const PA_MAJOR

minorVersionNumber :: Int
minorVersionNumber = #const PA_MINOR

microVersionNumber :: Int
microVersionNumber = #const PA_MICRO
