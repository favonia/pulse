{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Sample where

import Foreign

#include <pulse/sample.h>

{#enum sample_format as SampleFormat {underscoreToCase} deriving (Show, Eq) #}

data SampleSpec
{#pointer *sample_spec as SampleSpecPtr -> SampleSpec #}
