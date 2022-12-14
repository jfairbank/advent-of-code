{-# LANGUAGE LambdaCase #-}

module Day07
  ( Command (..),
    Dir (..),
    File (..),
    LogItem (..),
    Resource (..),
    parseCommand,
    puzzle1,
    puzzle2,
    parseFile,
    parseDir,
    parseLog,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Stack
import Text.Parsec
  ( ParseError,
    char,
    digit,
    endBy1,
    letter,
    many1,
    newline,
    parse,
    string,
  )
import Text.Parsec.String (Parser)

puzzle1 :: Text -> FileSize
puzzle1 input = fromRight 0 $ do
  log' <- parseLog $ Text.unpack input

  return $ sum $ filter (<= 100000) $ filesystemSizes $ processLog log'

puzzle2 :: Text -> FileSize
puzzle2 input = fromRight 0 $ do
  log' <- parseLog $ Text.unpack input

  let totalAvailable = 70000000
  let totalNeeded = 30000000
  let filesystem = processLog log'
  let totalUsed = dirSize filesystem 0 ["/"]
  let totalLeft = totalNeeded - (totalAvailable - totalUsed)

  return $ minimum $ filter (>= totalLeft) $ filesystemSizes filesystem

---- LOG ----

type Log = [LogItem]

data LogItem
  = LogCommand Command
  | LogFile File
  | LogDir Dir
  deriving (Eq, Show)

parseLog :: String -> Either ParseError Log
parseLog =
  parse (endBy1 logItemP newline) ""

logItemP :: Parser LogItem
logItemP =
  (LogCommand <$> commandP) <|> (LogFile <$> fileEntryP) <|> (LogDir <$> dirEntryP)

processLog :: Log -> Map String Resource
processLog log' =
  snd $
    foldl
      ( \(context, acc) item ->
          case item of
            LogCommand (Cd (Dir "..")) ->
              case Stack.pop context of
                Just (_, context') -> (context', acc)
                Nothing -> (context, acc)
            LogCommand (Cd dir@(Dir "/")) ->
              (Stack.singleton dir, acc)
            LogCommand (Cd dir) ->
              (Stack.push dir context, acc)
            LogCommand Ls ->
              (context, acc)
            LogFile file@(File name _) ->
              let parentKey = deriveParentKey context
                  parentDir = Dir parentKey
                  key = parentKey ++ "/" ++ name
               in ( context,
                    acc
                      & Map.insert key (ResourceFile file)
                      & Map.alter
                        ( \case
                            Just (ResourceDir dir resources) -> Just $ ResourceDir dir $ Set.insert key resources
                            _ -> Just $ ResourceDir parentDir $ Set.singleton key
                        )
                        parentKey
                  )
            LogDir (Dir name) ->
              let parentKey = deriveParentKey context
                  parentDir = Dir parentKey
                  key = parentKey ++ "/" ++ name
               in ( context,
                    acc
                      & Map.insertWith (\_ oldValue -> oldValue) key (ResourceDir (Dir key) Set.empty)
                      & Map.alter
                        ( \case
                            Just (ResourceDir dir' resources) -> Just $ ResourceDir dir' $ Set.insert key resources
                            _ -> Just $ ResourceDir parentDir $ Set.singleton key
                        )
                        parentKey
                  )
      )
      (Stack.empty, Map.empty)
      log'
  where
    deriveParentKey = intercalate "/" . reverse . fmap dirName

---- RESOURCE ----

data Resource
  = ResourceFile File
  | ResourceDir Dir (Set FilePath)
  deriving (Eq, Ord, Show)

---- COMMAND ----

data Command
  = Cd Dir
  | Ls
  deriving (Eq, Show)

parseCommand :: String -> Either ParseError Command
parseCommand =
  parse commandP ""

commandP :: Parser Command
commandP = do
  void $ char '$'
  spaceP
  cdP <|> lsP
  where
    cdP :: Parser Command
    cdP = Cd <$> (string "cd" >> spaceP >> dirP)

    lsP :: Parser Command
    lsP = Ls <$ string "ls"

---- FILE ----

type FileSize = Int

data File = File FilePath FileSize deriving (Eq, Ord, Show)

parseFile :: String -> Either ParseError File
parseFile =
  parse fileEntryP ""

fileEntryP :: Parser File
fileEntryP = do
  size <- read <$> many1 digit
  spaceP
  name <- many1 (char '.' <|> letter)

  return $ File name size

---- DIR ----

newtype Dir = Dir FilePath deriving (Eq, Ord, Show)

dirName :: Dir -> FilePath
dirName (Dir name) = name

parseDir :: String -> Either ParseError Dir
parseDir =
  parse dirEntryP ""

dirEntryP :: Parser Dir
dirEntryP = do
  void $ string "dir"
  spaceP
  dirP

dirP :: Parser Dir
dirP =
  Dir <$> (string ".." <|> string "/" <|> many1 letter)

---- PARSER HELPERS ----

spaceP :: Parser ()
spaceP = void $ many1 $ char ' '

---- FILESYSTEM ----

type Filesystem = Map FilePath Resource

filesystemSizes :: Filesystem -> [FileSize]
filesystemSizes filesystem =
  fmap (dirSize filesystem 0 . snd) $ mapMaybe resourceToMaybe $ Map.elems filesystem

dirSize :: Filesystem -> FileSize -> [FilePath] -> FileSize
dirSize _ acc [] = acc
dirSize filesystem acc (key : rest) =
  case Map.lookup key filesystem of
    Just (ResourceFile (File _ size)) ->
      dirSize filesystem (acc + size) rest
    Just (ResourceDir (Dir _) subResources) ->
      dirSize filesystem (dirSize filesystem acc (Set.elems subResources)) rest
    Nothing ->
      0

---- HELPERS ----

resourceToMaybe :: Resource -> Maybe (Dir, [FilePath])
resourceToMaybe (ResourceFile _) = Nothing
resourceToMaybe (ResourceDir dir resources) = Just (dir, Set.elems resources)
