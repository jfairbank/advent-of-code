module Logger (debug) where

import Debug.Trace (trace)

debug :: Show a => String -> a -> a
debug tag a =
  trace (tag ++ ": " ++ show a) a
