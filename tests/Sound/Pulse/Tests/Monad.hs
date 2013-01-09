{-# LANGUAGE OverloadedStrings #-}

module Sound.Pulse.Tests.Monad (tests) where

import Sound.Pulse.Monad
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit                       hiding (Test)


tests :: [Test]
tests = [testNewConnection]

testNewConnection :: Test
testNewConnection = testCase "monad/newConnection" $ do
    assertEqual "test" 1 1
