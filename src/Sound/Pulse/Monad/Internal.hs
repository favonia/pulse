{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

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
import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (MonadThrow(..),MonadCatch(..),MonadMask(..))

import Sound.Pulse.Monad.Internal.Connection

-- | The monad transformer wrapping oprations to a PulseAudio server.
newtype PulseT m n = PulseT { unPulseT :: ReaderT Context m n }
  deriving (Functor)

instance Applicative m => Applicative (PulseT m) where
    pure = PulseT . pure
    (<*>) (PulseT code1) (PulseT code2) = PulseT $ code1 <*> code2

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

instance MonadThrow m => MonadThrow (PulseT m) where
    throwM = PulseT . throwM

instance MonadCatch m => MonadCatch (PulseT m) where
    catch (PulseT code) handler = PulseT $ catch code (unPulseT . handler)

instance MonadMask m => MonadMask (PulseT m) where
    mask protected = PulseT $ mask $ \escape -> unPulseT $ protected (PulseT . escape . unPulseT)
    uninterruptibleMask protected = PulseT $ uninterruptibleMask $ \escape -> unPulseT $ protected (PulseT . escape . unPulseT)
