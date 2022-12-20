module Day09 (puzzle1, puzzle2) where

import Control.Monad (replicateM, void)
import Data.Biapplicative ((<<*>>))
import Data.Bifunctor (bimap, first, second)
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec
  ( ParseError,
    char,
    choice,
    digit,
    endBy1,
    many1,
    newline,
    parse,
    string,
  )
import Text.Parsec.String (Parser)
import Utils (listToPair)

type Knot = (Int, Int)

type Tail = Knot

type Rope = [Knot]

data Direction = U | R | D | L deriving (Show)

type Movement = (Direction, Int)

origin :: Knot
origin = (0, 0)

movementP :: Parser Movement
movementP = do
  direction <-
    choice
      [ U <$ string "U",
        R <$ string "R",
        D <$ string "D",
        L <$ string "L"
      ]
  void $ char ' '
  amount <- read <$> many1 digit

  return (direction, amount)

parseMovements :: Text -> Either ParseError [Movement]
parseMovements =
  parse (endBy1 movementP newline) "" . Text.unpack

moveRope :: Direction -> Rope -> Rope
moveRope _ [] = []
moveRope direction (ropeHead : rest) =
  followPreviousKnot (ropeHead, updatedRopeHead) [updatedRopeHead] rest
  where
    updatedRopeHead = moveKnot direction ropeHead

    followPreviousKnot _ acc [] = reverse acc
    followPreviousKnot (_, previousUpdatedKnotHead@(y1, x1)) acc (knot@(y2, x2) : rest') =
      followPreviousKnot (knot, updatedKnot) (updatedKnot : acc) rest'
      where
        dx = x1 - x2
        dy = y1 - y2

        updatedKnot =
          if isTouching previousUpdatedKnotHead knot
            then knot
            else
              ( y2 + (dy `safeDiv` abs dy),
                x2 + (dx `safeDiv` abs dx)
              )

applyAndTrackMovement :: Movement -> Rope -> (Rope, [Rope])
applyAndTrackMovement (direction, amount) rope =
  (last ropes, ropes)
  where
    ropes = take amount $ tail $ iterate (moveRope direction) rope

moveKnot :: Direction -> Knot -> Knot
moveKnot U = first (subtract 1)
moveKnot R = second (+ 1)
moveKnot D = first (+ 1)
moveKnot L = second (subtract 1)

isTouching :: Knot -> Knot -> Bool
isTouching knotHead knot =
  knot `elem` neighbors
  where
    neighborOffsets = mapMaybe listToPair $ replicateM 2 [-1, 0, 1]
    applyOffset = (<<*>>) . bimap (+) (+)
    neighbors = applyOffset <$> neighborOffsets <*> [knotHead]

getRopeTail :: Rope -> Tail
getRopeTail = last

moveAndCountTailVisits :: [Movement] -> Rope -> Int
moveAndCountTailVisits movements rope =
  countUnique $ getRopeTail <$> getTrackedRopes (applyMovements movements)
  where
    countUnique = Set.size . Set.fromList
    getTrackedRopes = snd
    applyMovements =
      foldl
        ( \(lastRope, trackedRopes) ->
            second (++ trackedRopes) . flip applyAndTrackMovement lastRope
        )
        (rope, [rope])

safeDiv :: Integral a => a -> a -> a
_ `safeDiv` 0 = 0
a `safeDiv` b = a `div` b

puzzle1 :: Text -> Int
puzzle1 input = fromRight 0 $ do
  movements <- parseMovements input
  return $ moveAndCountTailVisits movements $ replicate 2 origin

puzzle2 :: Text -> Int
puzzle2 input = fromRight 0 $ do
  movements <- parseMovements input
  return $ moveAndCountTailVisits movements $ replicate 10 origin
