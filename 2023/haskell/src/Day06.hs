module Day06 (puzzle1, puzzle2) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Parsec (digit, many1, newline, parse, sepBy1, string)
import Text.Parsec.Text (Parser)

int :: Parser Int
int = read <$> many1 digit

space :: Parser ()
space = void $ many1 $ string " "

parseRaces :: Parser [Int] -> Text -> [(Int, Int)]
parseRaces lineParser =
  fromMaybe [] . eitherToMaybe . parse racesParser ""
  where
    racesParser = do
      times <- string "Time:" >> space >> lineParser <* newline
      distances <- string "Distance:" >> space >> lineParser
      return $ zip times distances

puzzle :: Parser [Int] -> Text -> Int
puzzle lineParser input =
  product $ length . computeWinners <$> parseRaces lineParser input
  where
    compute maxTime (time, _) = (time, time * (maxTime - time))
    computeWinners (maxTime, recordDistance) =
      filter ((> recordDistance) . snd) $ take (maxTime + 1) $ iterate (compute maxTime . first (+ 1)) (0, 0)

puzzle1 :: Text -> Int
puzzle1 =
  puzzle $ sepBy1 int space

puzzle2 :: Text -> Int
puzzle2 =
  puzzle $ List.singleton . read . concat <$> sepBy1 (many1 digit) space
