{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{- |
Module      :  Sound.Pulse.Monad
Copyright   :  (c) MnO2
License     :  BSD3

Maintainer  :  mno2.csie@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides the monadic interface.
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
