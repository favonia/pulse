{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{- |
Module      :  Sound.Pulse.Monad
Copyright   :  (c) MnO2
License     :  BSD3

Maintainer  :  mno2.csie@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides the monadic interface.
-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Sample where

import Foreign

#include <pulse/sample.h>

{#enum sample_format as SampleFormat {underscoreToCase} deriving (Show, Eq) #}

data SampleSpec
{#pointer *sample_spec as SampleSpecPtr -> SampleSpec #}
