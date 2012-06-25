{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.TimeVal where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif

#include <pulse/timeval.h>

-- Timeval
data RawTimeVal
{#pointer *timeval as RawTimeValPtr -> RawTimeVal #}

