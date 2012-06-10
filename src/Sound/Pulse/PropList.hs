{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell #-}
{- |
Module      :  Sound.Pulse.PropList
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides the high-level property list interface.
-}
module Sound.Pulse.PropList
    ( AccessMode(..)
    , Bus(..)
    , Class(..)
    , FormFactor(..)
    , MouseButton(..)
    , Role(..)
    , PropTag(..)
    , PropList(..)
    ) where

import Data.Dependent.Map
import Sound.Pulse.PropList.Internal

-- |The tag type used to construct the map type 'PropList'.
-- This is a simple GDAT,
-- but Template Haskell can only generate it
-- in a more generalized syntax.
$(genPropTag)
$(deriveGEqPropTag)
$(deriveGComparePropTag)

-- |A map serving the high-level interface of @pa_proplist@
-- (<http://freedesktop.org/software/pulseaudio/doxygen/proplist_8h.html>).
type PropList = DMap PropTag

{-
-- |The tag type used to build the map.
data PropTag a where
  EventId :: PropTag String
  EventDescription :: PropTag String
  EventMouseX :: PropTag Int
  EventMouseY :: PropTag Int
  EventMouseHpos :: PropTag Double
  EventMouseVpos :: PropTag Double
  EventMouseButton :: PropTag MouseButton

  WindowName :: PropTag String
  WindowId :: PropTag String
  WindowIconName :: PropTag String
  WindowX :: PropTag Int
  WindowY :: PropTag Int
  WindowWidth :: PropTag Int
  WindowHeight :: PropTag Int
  WindowHpos :: PropTag Double
  WindowVpos :: PropTag Double
  WindowDesktop :: PropTag [Int]
  WindowX11Display :: PropTag String
  WindowX11Screen :: PropTag Int
  WindowX11Monitor :: PropTag Int
  WindowX11Xid :: PropTag Int -- Int???

  ApplicationName :: PropTag String
  ApplicationId :: PropTag String
  ApplicationVersion :: PropTag String
  ApplicationIconName :: PropTag String
  ApplicationLanguage :: PropTag String
  ApplicationProcessId :: PropTag Int -- process ID
  ApplicationProcessBinary :: PropTag String
  ApplicationProcessUser :: PropTag String
  ApplicationProcessHost :: PropTag String
  ApplicationProcessMachineId :: PropTag String
  ApplicationProcessSessionId :: PropTag String

  DeviceString :: PropTag String
  DeviceApi :: PropTag String
  DeviceDescription :: PropTag String
  DeviceBusPath :: PropTag String
  DeviceSerial :: PropTag String
  DeviceVendorId :: PropTag Int -- FIXME : ??
  DeviceVendorName :: PropTag String
  DeviceProductId :: PropTag Int -- FIXME : Device ID
  DeviceProductName :: PropTag String
  DeviceClass :: PropTag Class
  DeviceFormFactor :: PropTag String
  DeviceBus :: PropTag Bus
  DeviceIconName :: PropTag String
  DeviceAccessMode :: PropTag AccessMode
  DeviceMasterDevice :: PropTag String
  DeviceBufferingBufferSize :: PropTag Int
  DeviceBufferingFragmentSize :: PropTag Int
  DeviceProfileName :: PropTag String
  DeviceIntendedRoles :: PropTag [Role]
  DeviceProfileDescription :: PropTag String

  ModuleAuthor :: PropTag String
  ModuleDescription :: PropTag String
  ModuleUsage :: PropTag String
  ModuleVersion :: PropTag String

{- Not sure about this part
  FormatSampleFormat :: PropTag String -- PCM, pa_sample_format_to_string()
  FormatRate :: PropTag Int
  FormatChannels :: PropTag Int
  FormatChannelMap :: PropTag String -- PCM, pa_channel_map_snprint()
-}
-}