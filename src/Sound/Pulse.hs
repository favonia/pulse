{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{- |
This module just re-exports common submodules.
-}
module Sound.Pulse
    ( module Sound.Pulse.Monad
    , module Sound.Pulse.PropList
    , module Sound.Pulse.Volume
    ) where

import Sound.Pulse.Monad
import Sound.Pulse.PropList
import Sound.Pulse.Volume
