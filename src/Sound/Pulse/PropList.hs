{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
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
    (
    -- * High-level PropList
    PropList,
    module Data.Dependent.Map,
    PropTag(..),
    toKeyValue,
    fromKeyValue,
    AccessMode(..),
    Bus(..),
    Class(..),
    FormFactor(..),
    MouseButton(..),
    Role(..),
    Desktop,
    -- * Marshalling
    RawPropList,
    RawPropListPtr,
    peekRawPropList,
    withRawPropList,
    newRawPropList,
    freeRawPropList,
    parseString,
    ) where

import Prelude hiding (mapM_)
import Data.Maybe (fromJust)
import Data.Foldable (mapM_)
import System.IO (fixIO)
import Control.Monad hiding (mapM_)
import Control.Monad.CatchIO (MonadCatchIO(..), bracket, bracketOnError)
import Control.Monad.IO.Class (MonadIO(..))
#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
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
peekRawPropList :: MonadIO m => RawPropListPtr -> m PropList
peekRawPropList raw = liftIO $ with nullPtr $ \state -> do
    pl <- fixIO $ \loop -> do
        key' <- proplistIterate raw state
        case key' of
            Nothing -> return []
            Just key -> do
                -- XXX: 'fromJust' is ugly
                value <- liftM fromJust $ proplistGets raw key
                return $ fromKeyValue key value : loop
    return $ fromList pl

-- | Parse a string by the built-in parser @pa_proplist_from_string@.
parseString :: MonadCatchIO m => String -> m PropList
parseString strRep = bracket
    (liftIO $ proplistFromString strRep)
    (mapM_ $ liftIO . proplistFree)
    (\maybeP -> case maybeP of
        Nothing -> error "no parse"
        Just p -> peekRawPropList p)

-- | Marshal a 'PropList' into raw representation.
withRawPropList :: MonadCatchIO m => PropList -> (RawPropListPtr -> m a) -> m a
withRawPropList pl = bracket (newRawPropList pl) freeRawPropList

-- | Alloc a raw 'PropList'.
--   Users are responsible of using 'freeRawPropList' to free the resource.
newRawPropList :: MonadIO m => PropList -> m RawPropListPtr
newRawPropList pl = liftIO $ bracketOnError
    proplistNew
    proplistFree
    $ \rawPl -> do
        forM_ (toList pl) $ uncurry (proplistSets rawPl) . toKeyValue
        return rawPl

-- | Free the allocated raw 'PropList'.
freeRawPropList :: MonadIO m => RawPropListPtr -> m ()
freeRawPropList = liftIO . proplistFree
