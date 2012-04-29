{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.Pulse.Internal.Volume where

#include <pulse/volume.h>

type Volume = Integer
