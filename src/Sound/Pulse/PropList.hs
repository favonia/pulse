{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
This module provides the high-level property list interface.
-}
module Sound.Pulse.PropList
    (
    -- * High-level PropList
    PropList,
    module Data.Dependent.Map,
    PropTag(..),
    AccessMode(..),
    Bus(..),
    Class(..),
    FormFactor(..),
    MouseButton(..),
    Role(..),
    Desktop,
    -- * Marshalling
    toKeyValue,
    fromKeyValue,
    RawPropList,
    RawPropListPtr,
    peekRawPropList,
    withRawPropList,
    newRawPropList,
    freeRawPropList,
    parseString,
    ) where

import Prelude hiding (mapM_)
import Data.Foldable (mapM_)
import System.IO (fixIO)
import Control.Monad hiding (mapM_)
import Control.Monad.Catch (MonadMask, bracket, bracketOnError)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Safe
import Data.Dependent.Sum
import Data.Dependent.Map

import Sound.Pulse.Internal.PropList
import Sound.Pulse.PropList.Internal

-- | The tag type used to construct the map type 'PropList'.
--   This is equivalent to a simple GDAT,
--   but Template Haskell can only generate it
--   in a more generalized syntax.
$(genPropTag)
$(deriveGEqPropTag)
$(deriveEqTagPropTag)
$(deriveGComparePropTag)
$(deriveOrdTagPropTag)
$(deriveGShowPropTag)
$(deriveShowTagPropTag)

-- | Out marshaller for 'DSum' 'PropTag'.
$(genToKeyValue)

-- | In marshaller for 'DSum' 'PropTag'.
$(genFromKeyValue)

-- | A map serving the high-level interface of @pa_proplist@.
--   (See <http://freedesktop.org/software/pulseaudio/doxygen/proplist_8h.html>.)
type PropList = DMap PropTag

-- | Construct a 'PropList' out of the raw representation.
--   Might throw 'ErrorCall' for conversion failure.
peekRawPropList :: MonadIO m => RawPropListPtr -> m PropList
peekRawPropList raw = liftIO $ with nullPtr $ \state -> do
    pl <- fixIO $ \loop -> do
        key' <- proplistIterate raw state
        case key' of
            Nothing -> return []
            Just key -> do
                value <- proplistGet raw key
                return $ fromKeyValue key value : loop
    return $ fromList pl

-- | Parse a string by the built-in parser @pa_proplist_from_string@.
parseString :: (MonadMask m, MonadIO m) => String -> m PropList
parseString strRep = bracket
    (liftIO $ proplistFromString strRep)
    (mapM_ $ liftIO . proplistFree)
    (\maybeP -> case maybeP of
        Nothing -> error "no parse"
        Just p -> peekRawPropList p)

-- | Marshal a 'PropList' into raw representation.
withRawPropList :: (MonadMask m, MonadIO m) => PropList -> (RawPropListPtr -> m a) -> m a
withRawPropList pl = bracket (newRawPropList pl) freeRawPropList

-- | Alloc a raw 'PropList'.
--   Users are responsible of using 'freeRawPropList' to free the resource.
newRawPropList :: MonadIO m => PropList -> m RawPropListPtr
newRawPropList pl = liftIO $ bracketOnError
    proplistNew
    proplistFree
    $ \raw -> do
        forM_ (toList pl) $ uncurry (proplistSet raw) . toKeyValue
        return raw

-- | Free the allocated raw 'PropList'.
freeRawPropList :: MonadIO m => RawPropListPtr -> m ()
freeRawPropList = liftIO . proplistFree
