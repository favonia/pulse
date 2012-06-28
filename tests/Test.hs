
module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Sound.Pulse.Monad
import Test.QuickCheck

main = runPulseT defConfig code

code :: MonadIO a => PulseT a ()
code = liftIO $ putStrLn "[code]"
