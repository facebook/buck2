module Lib.TH ( mangle ) where

import Language.Haskell.TH

mangle :: Q [Dec] -> Q [Dec]
mangle decls = decls
