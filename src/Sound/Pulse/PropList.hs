{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, CPP #-}
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
    , StringList
    , PropTag(..)
    , readRawPropListPtr
    , writeRawPropListPtr
    ) where

import Data.Maybe (fromJust)
import System.IO (fixIO)
import Control.Monad
import Control.Monad.Trans (MonadIO(..))
#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Data.Dependent.Map

import Sound.Pulse.Internal.PropList
import Sound.Pulse.PropList.Internal

-- |The tag type used to construct the map type 'PropList'.
-- This is a simple GDAT,
-- but Template Haskell can only generate it
-- in a more generalized syntax.
$(genPropTag)
$(deriveGEqPropTag)
$(deriveGComparePropTag)

-- |Marshaling functions
$(genToKeyValue)
$(genFromKeyValue)

-- |A map serving the high-level interface of @pa_proplist@
-- (<http://freedesktop.org/software/pulseaudio/doxygen/proplist_8h.html>).
type PropList = DMap PropTag

-- |Construct a 'PropList' out of the raw representation.
readRawPropListPtr :: MonadIO m => RawPropListPtr -> m PropList
readRawPropListPtr raw = liftIO $ with nullPtr $ \state -> do
    pl <- fixIO $ \loop -> do
        key' <- proplistIterate raw state
        case key' of
            Nothing -> return []
            Just key -> do
                -- XXX: 'fromJust' is ugly
                value <- liftM fromJust $ proplistGets raw key
                return $ fromKeyValue key value : loop
    return $ fromList pl

-- |Marshal a 'PropList' into raw representation.
writeRawPropListPtr :: MonadIO m => RawPropListPtr -> PropList -> m ()
writeRawPropListPtr raw pl = liftIO $ do
    proplistClear raw
    forM_ (toList pl) $ uncurry (proplistSets raw) . toKeyValue

{-
-- |The tag type used to build the map.
data PropTag a where

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
