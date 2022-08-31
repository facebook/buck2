module Lib.Z (z) where

import Foreign.C

#include <buck2/tests/targets/rules/haskell/c.h>

newtype E = E Int deriving Enum

#{enum E, E
  , eA = eA
  , eB = eB }

z :: Int
z = #{size s} + #{offset s, b} + fromEnum eA + fromEnum eB + fromIntegral (f 3)

foreign import ccall f :: CInt -> CInt

g :: CInt -> CInt
g = f . (+1)

foreign export ccall "hs_g" g :: CInt -> CInt
