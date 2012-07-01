{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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
    StringList,
    -- * Marshalling
    RawPropList,
    RawPropListPtr,
    peekRawPropList,
    withRawPropList,
    newRawPropList,
    freeRawPropList,
    ) where

import Data.Maybe (fromJust)
import System.IO (fixIO)
import Control.Monad
import Control.Monad.CatchIO (MonadCatchIO(..), bracket)
import Control.Monad.IO.Class (MonadIO(..))
#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
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


parseRawPropList :: MonadIO m => String -> m PropList
parseRawPropList strRep = (liftIO $ proplistFromString strRep) >>= peekRawPropList


-- | Marshal a 'PropList' into raw representation.
withRawPropList :: MonadCatchIO m => PropList -> (RawPropListPtr -> m a) -> m a
withRawPropList pl = bracket (newRawPropList pl) freeRawPropList

-- | Alloc a raw 'PropList'.
--   Users are responsible of using 'freeRawPropList' to free the resource.
newRawPropList :: MonadIO m => PropList -> m RawPropListPtr
newRawPropList pl = liftIO $ do
    rawPl <- proplistNew
    forM_ (toList pl) $ uncurry (proplistSets rawPl) . toKeyValue
    return rawPl

-- | Free the allocated raw 'PropList'.
freeRawPropList :: MonadIO m => RawPropListPtr -> m ()
freeRawPropList = liftIO . proplistFree
