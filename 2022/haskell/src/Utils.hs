module Utils (listToPair, strip, textRead, textReadMaybe) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

listToPair :: [a] -> Maybe (a, a)
listToPair (x : y : _) = Just (x, y)
listToPair _ = Nothing

textRead :: Read a => Text -> a
textRead =
  read . Text.unpack

textReadMaybe :: Read a => Text -> Maybe a
textReadMaybe =
  readMaybe . Text.unpack

strip :: String -> String
strip =
  Text.unpack . Text.strip . Text.pack
