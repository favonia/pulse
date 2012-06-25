{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Sound.Pulse.Volume
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

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
