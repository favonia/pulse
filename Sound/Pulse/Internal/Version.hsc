module Sound.Pulse.Internal.Version where

#include <pulse/version.h>

apiVersion = #const PA_API_VERSION

protocolVersion = #const PA_PROTOCOL_VERSION

majorVersionNumber = #const PA_MAJOR

minorVersionNumber = #const PA_MINOR

microVersionNumber = #const PA_MICRO
