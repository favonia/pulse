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

module Sound.Pulse.Internal.Format where

import Foreign

#include <pulse/format.h>

{#enum encoding as Encoding {underscoreToCase} deriving (Show, Eq) #}
