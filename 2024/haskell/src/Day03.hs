{-# LANGUAGE NamedFieldPuns #-}

module Day03
  ( puzzle1,
    puzzle2,
  )
where

import Control.Applicative ((<|>))
import Data.Functor ((<$))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (anyChar, choice, digit, many1, parse, string, try)
import Text.Parsec.String (Parser)

data Mul = Mul {x :: !Int, y :: !Int} deriving (Show)

data DoState = Do | Dont deriving (Show)

data Instruction = DoStateInstruction !DoState | MulInstruction !Mul deriving (Show)

isMulInstruction :: Instruction -> Bool
isMulInstruction (MulInstruction _) = True
isMulInstruction _ = False

runProgram :: [Instruction] -> Int
runProgram instructions =
  helper instructions Do 0
  where
    helper [] _ acc = acc
    helper (MulInstruction Mul {x, y} : rest) Do acc = helper rest Do (acc + x * y)
    helper (MulInstruction _ : rest) Dont acc = helper rest Dont acc
    helper (DoStateInstruction Do : rest) _ acc = helper rest Do acc
    helper (DoStateInstruction Dont : rest) _ acc = helper rest Dont acc

intParser :: Parser Int
intParser = read <$> many1 digit

mulParser :: Parser Mul
mulParser = do
  string "mul("
  x <- intParser
  string ","
  y <- intParser
  string ")"
  return $ Mul x y

doStateParser :: Parser DoState
doStateParser =
  try (Do <$ string "do()") <|> try (Dont <$ string "don't()")

maybeInstructionParser :: Parser (Maybe Instruction)
maybeInstructionParser =
  choice
    [ Just . MulInstruction <$> try mulParser,
      Just . DoStateInstruction <$> try doStateParser,
      Nothing <$ anyChar
    ]

parseInstructions :: Text -> [Instruction]
parseInstructions =
  either (const []) catMaybes . parse (many1 maybeInstructionParser) "" . Text.unpack . Text.strip

puzzle1 :: Text -> Int
puzzle1 =
  runProgram . filter isMulInstruction . parseInstructions

puzzle2 :: Text -> Int
puzzle2 =
  runProgram . parseInstructions
