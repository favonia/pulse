{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{- |
This module provides the internals of the monadic interface.
We want to hide the state and forbid unsafe forking
in the public interface, but expose everything here.
-}
module Sound.Pulse.Monad.Internal
    (
    PulseT(..),
    ) where

import Prelude hiding (catch)
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.CatchIO (MonadCatchIO(..))

import Sound.Pulse.Monad.Internal.Connection

-- | The monad transformer wrapping oprations to a PulseAudio server.
newtype PulseT m n = PulseT { unPulseT :: ReaderT Context m n }

instance Functor m => Functor (PulseT m) where
    fmap f (PulseT x) = PulseT $ fmap f x

instance Monad m => Monad (PulseT m) where
    PulseT f >>= g = PulseT $ f >>= unPulseT . g
    PulseT f >> PulseT g = PulseT $ f >> g
    fail = PulseT . fail
    return = PulseT . return

instance MonadIO m => MonadIO (PulseT m) where
    liftIO = PulseT . liftIO

instance MonadPlus m => MonadPlus (PulseT m) where
    mzero = PulseT mzero
    mplus (PulseT x) (PulseT y) = PulseT $ mplus x y

instance MonadTrans PulseT where
    lift = PulseT . lift

instance MonadCatchIO m => MonadCatchIO (PulseT m) where
    catch (PulseT code) handler = PulseT $ catch code (unPulseT . handler)
    block (PulseT code) = PulseT $ block code
    unblock (PulseT code) = PulseT $ unblock code
