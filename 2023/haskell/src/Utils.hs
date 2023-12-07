module Utils
  ( debugLog,
  )
where

import Debug.Trace (trace)

debugLog :: (Show a) => String -> a -> a
debugLog tag value =
  trace (tag ++ ": " ++ show value) value
