{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{- |
This module provides the utilities for volume.
-}
module Sound.Pulse.Volume
    ( Volume
    , volumeMuted
    , volumeNorm
    ) where

import Sound.Pulse.Internal.Volume (Volume)

-- | The normal volume (100%)
volumeNorm :: Volume
volumeNorm = undefined

-- | The muted volume (0%)
volumeMuted :: Volume
volumeMuted = undefined
