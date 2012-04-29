
module Main where

import Sound.Pulse
import Test.QuickCheck

main = quickCheck ((==0).(`mod`2).(*2) :: Int -> Bool)
