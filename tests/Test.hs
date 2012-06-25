
module Main where

import Sound.Pulse.Monad
import Test.QuickCheck

main = do
  runPulse DefaultServer DoNotWaitForDaemon code
  runPulse DefaultServer WaitForDaemon code

code :: Monad a => Pulse a ()
code = return ()
