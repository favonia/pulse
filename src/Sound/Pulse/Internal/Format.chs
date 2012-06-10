{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Format where

import Foreign

#include <pulse/format.h>

{#enum encoding as Encoding {underscoreToCase} deriving (Show, Eq) #}
