{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
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

-- | Run the 'PulseT'.
runPulseT :: MonadCatchIO m => Config -> PulseT m n -> m n
runPulseT conf (PulseT (ReaderT userCode)) = bracket (liftIO $ newConn conf) (liftIO . freeConn) userCode
