{-# LANGUAGE ForeignFunctionInterface #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Volume where

#include <pulse/volume.h>

type Volume = Integer
