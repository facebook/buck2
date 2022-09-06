{-# LANGUAGE TemplateHaskell #-}
module TestTemplateHaskell (main) where

import Lib.TH

$(mangle
  [d|
    x :: String
    x = "Hello, World!"
  |])

main :: IO ()
main = putStrLn x
