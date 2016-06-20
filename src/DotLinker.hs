{-# LANGUAGE OverloadedStrings #-}
module DotLinker where

import Data.Attoparsec.ByteString.Char8
import Data.Functor (void)
import Data.Text (Text, pack)
import Filesystem.Path.CurrentOS (FilePath, decode)
import Prelude hiding (takeWhile, FilePath)

data Entry = Entry Text [FilePath]
  deriving (Eq, Show)

-- | Parser for the map file.
lineMapParser :: Parser Entry
lineMapParser = do
    src <- word
    void $ char ':'
    skipMany space
    ts <- filePath `sepBy` char ','
    return $ Entry (pack src) ts
  where
    word = many' letter_iso8859_15
    filePath =
      strip $ decode <$> takeWhile (/= ',')

    strip str = many' space *> str <* many' space

fileMapParser :: Parser [Entry]
fileMapParser = many' $ lineMapParser <* endOfLine
