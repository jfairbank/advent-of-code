module Stack (Stack, empty, pop, popMany, push) where

import Data.List (uncons)

type Stack a = [a]

pop :: Stack a -> Maybe (a, Stack a)
pop = uncons

popMany :: Int -> Stack a -> ([a], Stack a)
popMany = splitAt

push :: a -> Stack a -> Stack a
push = (:)

empty :: Stack a
empty = []
