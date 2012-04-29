{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Sound.Pulse.Internal.Sample where

import Foreign.Ptr

#include <pulse/sample.h>

{#enum pa_sample_format as SampleFormat {underscoreToCase} deriving (Show, Eq) #}

data SampleSpec
{#pointer *pa_sample_spec as SampleSpecPtr -> SampleSpec #}
