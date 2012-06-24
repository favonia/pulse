{-# LANGUAGE Safe #-}
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
