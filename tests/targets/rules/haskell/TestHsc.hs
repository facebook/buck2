module TestHsc (main) where

import Test.HUnit
import TestRunner

import Lib.Z (z)

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "hsc" $ TestCase $ assertEqual "z" 17 z
  ]
