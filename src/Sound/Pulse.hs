{- |
Module      :  Sound.Pulse
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This file re-exports common submodules.
-}
module Sound.Pulse
    ( module Sound.Pulse.Monad
    , module Sound.Pulse.PropList
    , module Sound.Pulse.Volume
    ) where

import Sound.Pulse.Monad
import Sound.Pulse.PropList
import Sound.Pulse.Volume
