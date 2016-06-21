{-# LANGUAGE OverloadedStrings #-}
module DotLinker
( lineMapParser
, fileMapParser
, Entry(..)
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Text (Text, pack)
import Filesystem.Path.CurrentOS (FilePath, decode)
import Prelude hiding (takeWhile, FilePath)

data Entry = Entry Text [FilePath]
  deriving (Eq, Show)

-- | Parser for the map file.
lineMapParser :: Parser Entry
lineMapParser = do
    src <- word <* char ':'
    ts <- filePath `sepBy1` char ','
    return $ Entry (pack src) ts
  where
    word = many' letter_iso8859_15
    filePath =
      strip $ decode <$> takeTill (inClass ",\n\r")

    strip str = many' space *> str <* many' space

fileMapParser :: Parser [Entry]
fileMapParser = manyTill lineMapParser endOfInput
