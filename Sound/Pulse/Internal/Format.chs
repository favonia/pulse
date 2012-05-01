{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Format where

import Foreign.Ptr

#include <pulse/format.h>

{#enum pa_encoding as Encoding {underscoreToCase} deriving (Show, Eq) #}
