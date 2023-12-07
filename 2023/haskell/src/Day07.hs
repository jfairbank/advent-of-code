{-# LANGUAGE FlexibleInstances #-}

module Day07 (puzzle1, puzzle2) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Either.Extra (eitherToMaybe)
import Data.Functor (($>))
import qualified Data.List as List
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Parsec (choice, digit, many, many1, newline, parse, sepEndBy1, string)
import Text.Parsec.Text (Parser)

int :: Parser Int
int = read <$> many1 digit

space :: Parser ()
space = void $ many1 $ string " "

optionalSpace :: Parser ()
optionalSpace = void $ many $ string " "

sortBy :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy f = List.sortBy (\x y -> compare (f x) (f y))

data Card = WildcardJack | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | RegularJack | Q | K | A deriving (Eq, Ord)

newtype Hand_ a = Hand (a, a, a, a, a)

type Hand = Hand_ Card

data HandType = HighCard | OnePair | TwoPair | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind deriving (Eq, Ord)

type Bet = Int

type Round = (Hand, Bet)

type Game = [Round]

instance Foldable Hand_ where
  foldr f init = foldr f init . handToList

instance Eq Hand where
  a == b = List.sort (handToList a) == List.sort (handToList b)

instance Ord Hand where
  compare a b =
    case compare (toHandType a) (toHandType b) of
      EQ -> helper (handToList a) (handToList b)
      ordering -> ordering
    where
      helper [] _ = EQ
      helper _ [] = EQ
      helper (x : xs) (y : ys) =
        case compare x y of
          EQ -> helper xs ys
          ordering -> ordering

handToList :: Hand_ a -> [a]
handToList (Hand (a, b, c, d, e)) = [a, b, c, d, e]

toHandType :: Hand -> HandType
toHandType hand =
  case counts of
    [_] -> FiveOfKind
    [1, 4] -> FourOfKind
    [2, 3] -> FullHouse
    [1, 1, 3] -> ThreeOfKind
    [1, 2, 2] -> TwoPair
    [1, 1, 1, 2] -> OnePair
    _ -> HighCard
  where
    initialCounts = foldr (Map.alter (\maybeCount -> ((+ 1) <$> maybeCount) <|> Just 1)) Map.empty hand
    wildcardCount = fromMaybe 0 $ initialCounts !? WildcardJack
    counts = useWildcards $ List.sort $ Map.elems $ Map.delete WildcardJack initialCounts

    useWildcards [] = [5]
    useWildcards [lastCount] = [lastCount + wildcardCount]
    useWildcards (x : xs) = x : useWildcards xs

cardParser :: Card -> Parser Card
cardParser jackType =
  choice
    [ string "A" $> A,
      string "K" $> K,
      string "Q" $> Q,
      string "J" $> jackType,
      string "T" $> Ten,
      string "9" $> Nine,
      string "8" $> Eight,
      string "7" $> Seven,
      string "6" $> Six,
      string "5" $> Five,
      string "4" $> Four,
      string "3" $> Three,
      string "2" $> Two
    ]

handParser :: Card -> Parser Hand
handParser jackType = do
  values <- (,,,,) <$> cardParser' <*> cardParser' <*> cardParser' <*> cardParser' <*> cardParser'
  return $ Hand values
  where
    cardParser' = cardParser jackType

roundParser :: Card -> Parser Round
roundParser jackType =
  (,) <$> (handParser jackType <* space) <*> (int <* optionalSpace)

gameParser :: Card -> Parser Game
gameParser jackType =
  sepEndBy1 (roundParser jackType) newline

parseGame :: Card -> Text -> Game
parseGame jackType =
  fromMaybe [] . eitherToMaybe . parse (gameParser jackType) ""

solveWith :: Card -> Text -> Int
solveWith jackType =
  snd . foldl roundWinnings (1, 0) . sortBy fst . parseGame jackType
  where
    roundWinnings (rank, total) (_, bet) = (rank + 1, rank * bet + total)

puzzle1 :: Text -> Int
puzzle1 =
  solveWith RegularJack

puzzle2 :: Text -> Int
puzzle2 =
  solveWith WildcardJack
