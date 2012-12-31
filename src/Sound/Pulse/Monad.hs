{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

{- |
This module provides the monadic interface.
-}
module Sound.Pulse.Monad
    (
    -- * Monad Transformer
    PulseT,
    runPulseT,
    -- * Configuration
    Config(..),
    defConfig,
    ServerName(..),
    ConnMode(..),
    ) where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.CatchIO (MonadCatchIO(..), bracket)

import Sound.Pulse.Monad.Internal
import Sound.Pulse.Monad.Internal.Connection
import Sound.Pulse.Monad.Internal.Introspect

-- | Run the 'PulseT'.
runPulseT :: MonadCatchIO m => Config -> PulseT m n -> m n
runPulseT conf (PulseT (ReaderT userCode)) = bracket (liftIO $ newConn conf) (liftIO . freeConn) userCode
