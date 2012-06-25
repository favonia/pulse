{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Sound.Pulse.Monad
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides the monadic interface.
-}
module Sound.Pulse.Monad
    ( ServerName (..)
    , ConnectionMode (..)
    , Pulse
    , runPulse
    ) where

import Control.Monad.State
import Data.String (IsString(..))

import Sound.Pulse.Internal.Context

-- | The name of the server the monad is connecting to.
data ServerName = DefaultServer | Named String
    deriving (Eq, Ord, Show)

instance IsString ServerName where
    fromString = Named

-- | The mode of connection.
data ConnectionMode = WaitForDaemon       -- Wait for the daemon to appear.
                    | DoNotWaitForDaemon  -- Fails right away.

-- | The type of the context. TODO: Fill in something real here.
newtype Context = Context ()

-- | The monad wrapping oprations to a PulseAudio server.
newtype Pulse m n = Pulse (StateT Context m n)
    deriving (Functor, Monad, MonadFix, MonadIO, MonadPlus)

-- | Run the oprations against the server.
runPulse :: Monad m => ServerName -> ConnectionMode -> Pulse m n -> m n
runPulse name mode (Pulse code) = evalStateT code (Context ())
