module Stack
  ( Stack,
    empty,
    pop,
    popMany,
    push,
    singleton,
  )
where

import Data.List (uncons)
import qualified Data.List as List

type Stack a = [a]

singleton :: a -> [a]
singleton = List.singleton

pop :: Stack a -> Maybe (a, Stack a)
pop = uncons

popMany :: Int -> Stack a -> ([a], Stack a)
popMany = splitAt

push :: a -> Stack a -> Stack a
push = (:)

empty :: Stack a
empty = []
