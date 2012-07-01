{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{- |
Module      :  Sound.Pulse.Monad.Internal.Connection
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

High-level interface for context.
-}

module Sound.Pulse.Monad.Internal.Connection
    (
    -- Configuration
    Config(..),
    defConfig,
    ServerName(..),
    ConnMode(..),
    -- Connection
    Context,
    ctxRaw,
    newConn,
    freeConn,
    ConnectionFail,
    ) where

import Prelude hiding (catch)

import Data.Typeable
import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Control.Concurrent.STM
import Control.Exception
#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe hiding (void)
#else
import Foreign hiding (void)
#endif
import Data.String (IsString(..))

import Sound.Pulse.Internal.Def
import Sound.Pulse.Internal.Context
import Sound.Pulse.Internal.Operation
import Sound.Pulse.Internal.C2HS

import Sound.Pulse.PropList
import Sound.Pulse.Monad.Internal.MainLoop

-- | The name of the server the monad is connecting to.
data ServerName = DefaultServer | Named String
    deriving (Eq, Ord, Show)

instance IsString ServerName where
    fromString = Named

-- | The mode of connection.
data ConnMode = WaitForDaemon       -- ^ Wait for the daemon to appear.
              | DoNotWaitForDaemon  -- ^ Fails right away.

-- | Marshalling functions.
fromServerName :: ServerName -> Maybe String
fromServerName DefaultServer = Nothing
fromServerName (Named s) = Just s

-- | Marshalling functions.
fromMode :: ConnMode -> [ContextFlags]
fromMode WaitForDaemon = [ContextNofail, ContextNoautospawn]
fromMode DoNotWaitForDaemon = [ContextNoautospawn]

-- | Connection setting.
data Config = Config
    { confServerName :: ServerName
    , confAppName :: String
    , confProp :: PropList
    , confMode :: ConnMode
    }

-- | Default connection setting.
defConfig :: Config
defConfig = Config
    { confServerName = DefaultServer
    , confAppName = ""
    , confProp = empty
    , confMode = DoNotWaitForDaemon
    }

-- | The type of the context.
data Context = Context
    { ctxRaw :: RawContextPtr
    , ctxDead :: TVar Bool
    , ctxState :: StablePtr (TVar ContextState)
    , ctxLoop :: MainLoop
    }

-- | Callback for state changes
cxtStateCallback :: RawContextNotifyCallback a
cxtStateCallback raw monPtr' =
    forM_ (castPtrToMaybeStable monPtr') $ \monPtr -> do
        mon <- deRefStablePtr monPtr
        atomically . writeTVar mon =<< contextGetState raw

-- | Ugly trick to create a static C wrapper.
foreign export ccall "_pulse_private_cxtStateCallback"
    cxtStateCallback :: RawContextNotifyCallback a
foreign import ccall "&_pulse_private_cxtStateCallback"
    wrappedCxtStateCallback :: FunPtr (RawContextNotifyCallback a)

-- | Setting up the connection. Might throw 'ConnectionFail'.
newConn :: Config -> IO Context
newConn conf = mask_ $ bracketOnError
    newLoop
    freeLoop
    $ \loop -> bracketOnError
        (do
            loopApi <- getApi loop
            withRawPropList (confProp conf) $ contextNewWithProplist loopApi (confAppName conf))
        contextUnref
        $ \raw -> bracketOnError
            (do
                mon <- newTVarIO =<< contextGetState raw
                monPtr <- newStablePtr mon
                return (mon, monPtr))
            (freeStablePtr . snd)
            $ \(mon, monPtr) -> do
                contextSetStateCallback raw wrappedCxtStateCallback (Just monPtr)
                ret <- contextConnect raw (fromServerName $ confServerName conf) (fromMode $ confMode conf) nullPtr
                when (ret /= 0) $ throwIO ConnectionFail
                bracketOnError
                    (runLoop loop)
                    (const $ quitLoop loop)
                    $ const $ do
                        atomically $ do
                            state <- readTVar mon
                            case state of
                                ContextUnconnected -> retry
                                ContextConnecting -> retry
                                ContextAuthorizing -> retry
                                ContextSettingName -> retry
                                ContextReady -> return () -- Yay!
                                ContextFailed -> throwSTM ConnectionFail
                                ContextTerminated -> throwSTM ConnectionFail
                        dead <- newTVarIO False
                        return Context
                            { ctxRaw = raw
                            , ctxDead = dead
                            , ctxLoop = loop
                            , ctxState = monPtr
                            }

-- | Callback for draining.
cxtDrainCallback :: RawContextNotifyCallback a
cxtDrainCallback raw _ = contextDisconnect raw

-- | Ugly trick to create a static C wrapper.
foreign export ccall "_pulse_private_cxtDrainCallback"
    cxtDrainCallback :: RawContextNotifyCallback a
foreign import ccall "&_pulse_private_cxtDrainCallback"
    wrappedCxtDrainCallback :: FunPtr (RawContextNotifyCallback a)

-- | End the connection and free the resource.
freeConn :: Context -> IO ()
freeConn ctx = mask_ $ do
    -- ctxDead
    atomically $ do
        void . check . not =<< readTVar (ctxDead ctx)
        writeTVar (ctxDead ctx) True
    -- ctxRaw
    maybe (contextDisconnect $ ctxRaw ctx) operationUnref
        =<< blockLoop (ctxLoop ctx) (contextDrain (ctxRaw ctx) wrappedCxtDrainCallback Nothing)
    -- ctxState
    freeStablePtr (ctxState ctx)
    -- ctxLoop and ctxRaw
    quitLoop (ctxLoop ctx)
    freeLoop (ctxLoop ctx)
    contextUnref (ctxRaw ctx)

-------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------

-- | Connection fails for some reason.
data ConnectionFail = ConnectionFail deriving (Show, Typeable)

instance Exception ConnectionFail where
