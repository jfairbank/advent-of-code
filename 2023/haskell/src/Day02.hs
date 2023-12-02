module Day02 (puzzle1, puzzle2) where

import Data.Either.Extra (eitherToMaybe)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShow)
import Text.Parsec (choice, digit, many1, parse, sepBy1, space, string)
import Text.Parsec.String (Parser)

-- HELPERS --

int :: Parser Int
int = read <$> many1 digit

-- ROUND --

data Round = Round {red :: Int, green :: Int, blue :: Int}

roundParser :: Parser Round
roundParser =
  foldr addToRound newRound <$> sepBy1 cubeParser (string ", ")
  where
    addToRound (RedCube count) round = round {red = red round + count}
    addToRound (GreenCube count) round = round {green = green round + count}
    addToRound (BlueCube count) round = round {blue = blue round + count}

newRound :: Round
newRound = Round 0 0 0

-- CUBE --

data Cube = RedCube Int | GreenCube Int | BlueCube Int

cubeParser :: Parser Cube
cubeParser = do
  count <- int
  space
  choice
    [ string "red" $> RedCube count,
      string "green" $> GreenCube count,
      string "blue" $> BlueCube count
    ]

-- GAME --

data Game = Game {gameId :: Int, rounds :: [Round]}

gameParser :: Parser Game
gameParser = do
  string "Game "
  id <- int
  string ": "
  Game id <$> sepBy1 roundParser (string "; ")

parseGames :: Text -> [Game]
parseGames =
  mapMaybe (eitherToMaybe . parse gameParser "") . lines . Text.unpack

-- PUZZLES --

puzzle1 :: Text -> Int
puzzle1 input =
  sum $ gameId <$> filter (all isValidRound . rounds) (parseGames input)
  where
    isValidRound round = red round <= 12 && green round <= 13 && blue round <= 14

puzzle2 :: Text -> Int
puzzle2 input =
  sum $ computePower . findMinimumNeeded <$> parseGames input
  where
    findMinimumNeeded = foldl maxOfRounds newRound . rounds
    computePower round = red round * green round * blue round
    maxOfRounds round1 round2 =
      Round
        { red = max (red round1) (red round2),
          green = max (green round1) (green round2),
          blue = max (blue round1) (blue round2)
        }
