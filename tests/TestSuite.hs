module Main where

import qualified Sound.Pulse.Tests.Monad as Monad
import           Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
    where
        tests = [ testGroup "Tests.Monad" Monad.tests
                ]
