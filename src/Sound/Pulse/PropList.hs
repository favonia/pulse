{-# LANGUAGE Trustworthy #-}
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
    , StringList
    , PropTag(..)
    , PropList
    , peekRawPropList
    , withRawPropList
    ) where

import Data.Maybe (fromJust)
import System.IO (fixIO)
import Control.Monad
import Control.Exception.Base (bracket)
import Foreign.Safe
import Data.Dependent.Map

import Sound.Pulse.Internal.PropList
import Sound.Pulse.PropList.Internal

-- | The tag type used to construct the map type 'PropList'.
--   This is equivalent to a simple GDAT,
--   but Template Haskell can only generate it
--   in a more generalized syntax.
$(genPropTag)
$(deriveGEqPropTag)
$(deriveGComparePropTag)

-- | Marshaling functions
$(genToKeyValue)
$(genFromKeyValue)

-- | A map serving the high-level interface of @pa_proplist@.
--   (See <http://freedesktop.org/software/pulseaudio/doxygen/proplist_8h.html>.)
type PropList = DMap PropTag

-- | Construct a 'PropList' out of the raw representation.
peekRawPropList :: RawPropListPtr -> IO PropList
peekRawPropList raw = with nullPtr $ \state -> do
    pl <- fixIO $ \loop -> do
        key' <- proplistIterate raw state
        case key' of
            Nothing -> return []
            Just key -> do
                -- XXX: 'fromJust' is ugly
                value <- liftM fromJust $ proplistGets raw key
                return $ fromKeyValue key value : loop
    return $ fromList pl

-- | Marshal a 'PropList' into raw representation.
withRawPropList :: PropList -> (RawPropListPtr -> IO a) -> IO a
withRawPropList pl code =
    bracket
        (do
            rawPl <- proplistNew
            forM_ (toList pl) $ uncurry (proplistSets rawPl) . toKeyValue
            return rawPl
        )
        proplistFree
        code

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
