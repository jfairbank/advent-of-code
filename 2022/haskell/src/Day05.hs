{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05 (puzzle1, puzzle2) where

import Control.Monad (void)
import Data.Array (Array, elems, listArray, (!), (//))
import Data.Either.Extra (eitherToMaybe)
import Data.List.Extra (chunksOf, splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Stack (Stack)
import qualified Stack
import Text.Parsec (char, digit, letter, many1, parse, string)
import Text.Parsec.String (Parser)
import qualified Utils

---- GENERIC PARSERs ----

intP :: Parser Int
intP = read <$> many1 digit

spaceP :: Parser ()
spaceP = void $ many1 $ char ' '

---- MOVE ----

data Move = Move
  { quantity :: Int,
    from :: Int,
    to :: Int
  }

parseMove :: String -> Maybe Move
parseMove =
  eitherToMaybe . parse parser ""
  where
    parser = do
      quantity' <- string "move" >> spaceP >> intP
      from' <- spaceP >> string "from" >> spaceP >> intP
      to' <- spaceP >> string "to" >> spaceP >> intP

      return $ Move quantity' from' to'

---- CRATES ----

type Crate = Char

parseCrate :: String -> Maybe Crate
parseCrate =
  eitherToMaybe . parse parser ""
  where
    parser = do
      void $ char '['
      char' <- letter
      void $ char ']'
      return char'

---- STACKS ----

type Stacks = Array Int (Stack Crate)

parseStacks :: String -> Stacks
parseStacks input =
  foldl
    ( \acc row ->
        fst $
          foldl
            ( \(acc', i) crate ->
                let stack = getStack i acc'
                    updatedStack = maybe stack (`Stack.push` stack) $ parseCrate crate
                 in (setStack i updatedStack acc', i + 1)
            )
            (acc, 0)
            row
    )
    stacks
    rawRows
  where
    rawRows = tail $ reverse $ fmap Utils.strip . chunksOf 4 <$> splitOn "\n" input
    numStacks = length $ head rawRows
    stacks = listArray (0, numStacks - 1) $ repeat Stack.empty

getStack :: Int -> Stacks -> Stack Crate
getStack = flip (!)

setStack :: Int -> Stack Crate -> Stacks -> Stacks
setStack i stack stacks = stacks // [(i, stack)]

applyMoves :: (Move -> Stacks -> Stacks) -> [Move] -> Stacks -> Stacks
applyMoves f moves stacks = foldl (flip f) stacks moves

applyMove :: (Stack Crate -> [Crate] -> Stack Crate) -> Move -> Stacks -> Stacks
applyMove apply move stacks =
  setStack toIndex updatedDestination $ setStack fromIndex updatedSource stacks
  where
    fromIndex = move.from - 1
    toIndex = move.to - 1
    source = getStack fromIndex stacks
    destination = getStack toIndex stacks
    (values, updatedSource) = Stack.popMany move.quantity source
    updatedDestination = apply destination values

applyMoveAsStack :: Move -> Stacks -> Stacks
applyMoveAsStack = applyMove $ foldl $ flip Stack.push

applyMoveAsQueue :: Move -> Stacks -> Stacks
applyMoveAsQueue = applyMove $ foldr Stack.push

---- PUZZLES ----

puzzle1 :: Text -> String
puzzle1 =
  parseAndApplyMoves applyMoveAsStack

puzzle2 :: Text -> String
puzzle2 =
  parseAndApplyMoves applyMoveAsQueue

parseAndApplyMoves :: (Move -> Stacks -> Stacks) -> Text -> String
parseAndApplyMoves applyMove' input = fromMaybe "" $ do
  (stacks, moves) <- parseInput
  let updatedStacks = applyMoves applyMove' moves stacks
  mapM (fmap fst . Stack.pop) $ elems updatedStacks
  where
    parseInput :: Maybe (Stacks, [Move])
    parseInput =
      case splitOn "\n\n" $ Text.unpack $ Text.dropWhile (== '\n') input of
        [stacksInput, moveInput] ->
          Just
            ( parseStacks stacksInput,
              mapMaybe parseMove $ splitOn "\n" moveInput
            )
        _ ->
          Nothing
