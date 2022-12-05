{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day02 (Score (..), puzzle1, puzzle2) where

import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Utils

---- SHAPE ----

data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

instance Enum Shape where
  toEnum 0 = Rock
  toEnum 1 = Paper
  toEnum 2 = Scissors
  toEnum n = error ("Invalid enum value for Shape: " ++ show n)

  fromEnum Rock = 0
  fromEnum Paper = 1
  fromEnum Scissors = 2

  succ Scissors = Rock
  succ shape = toEnum (fromEnum shape + 1)

  pred Rock = Scissors
  pred shape = toEnum (fromEnum shape - 1)

instance Ord Shape where
  compare Rock Scissors = GT
  compare Scissors Rock = LT
  compare shape1 shape2 = compare (fromEnum shape1) (fromEnum shape2)

---- PLAYER ----

newtype Player = Player Shape

instance Read Player where
  readsPrec _ "X" = [(Player Rock, "")]
  readsPrec _ "Y" = [(Player Paper, "")]
  readsPrec _ "Z" = [(Player Scissors, "")]
  readsPrec _ _ = []

---- PLAYER REACTION ----

newtype PlayerReaction = PlayerReaction Outcome

instance Read PlayerReaction where
  readsPrec _ "X" = [(PlayerReaction Loss, "")]
  readsPrec _ "Y" = [(PlayerReaction Draw, "")]
  readsPrec _ "Z" = [(PlayerReaction Win, "")]
  readsPrec _ _ = []

---- OPPONENT ----

newtype Opponent = Opponent Shape

instance Read Opponent where
  readsPrec _ "A" = [(Opponent Rock, "")]
  readsPrec _ "B" = [(Opponent Paper, "")]
  readsPrec _ "C" = [(Opponent Scissors, "")]
  readsPrec _ _ = []

---- SCORE ----

newtype Score = Score Int deriving (Eq)

instance Show Score where
  show (Score x) = show x

instance Semigroup Score where
  (Score x) <> (Score y) = Score (x + y)

instance Monoid Score where
  mempty = Score 0

scoreShape :: Shape -> Score
scoreShape = Score . (1 +) . fromEnum

---- OUTCOME ----

data Outcome
  = Loss
  | Draw
  | Win
  deriving (Enum, Show)

scoreOutcome :: Outcome -> Score
scoreOutcome = Score . (3 *) . fromEnum

---- GAME ----

checkOutcome :: Opponent -> Player -> Outcome
checkOutcome (Opponent opponentShape) (Player playerShape) =
  case compare playerShape opponentShape of
    GT -> Win
    LT -> Loss
    EQ -> Draw

scoreGame :: Opponent -> Player -> Score
scoreGame opponent player@(Player playerShape) =
  scoreShape playerShape <> scoreOutcome outcome
  where
    outcome = checkOutcome opponent player

---- HELPERS ----

parseLine :: (Read a, Read b) => Text -> Maybe (a, b)
parseLine line =
  bimap read read <$> Utils.listToPair (Text.unpack <$> Text.splitOn " " line)

stripInput :: Text -> [Text]
stripInput =
  fmap Text.strip . Text.lines . Text.strip

---- PUZZLES ----

puzzle1 :: Text -> Score
puzzle1 =
  foldMap scoreLine . stripInput
  where
    scoreLine :: Text -> Score
    scoreLine line = fromMaybe (Score 0) $ do
      uncurry scoreGame <$> parseLine @Opponent @Player line

puzzle2 :: Text -> Score
puzzle2 =
  foldMap scoreLine . stripInput
  where
    scoreLine :: Text -> Score
    scoreLine line = fromMaybe (Score 0) $ do
      (opponent@(Opponent opponentShape), PlayerReaction outcome) <-
        parseLine @Opponent @PlayerReaction line

      let player = Player $
            case outcome of
              Loss -> pred opponentShape
              Draw -> opponentShape
              Win -> succ opponentShape

      return $ scoreGame opponent player
