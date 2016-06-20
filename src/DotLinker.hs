{-# LANGUAGE OverloadedStrings #-}
module DotLinker where

import Data.Attoparsec.ByteString.Char8
import Data.Text (Text, pack)

data Entry = Entry Text [FilePath]

-- | Parser for the map file.
fileMapParser :: Parser Entry
fileMapParser = do
    src <- word
    char ':'
    skipMany space
    ts <- word `sepBy` char ','
    return $ Entry (pack src) ts
  where
    word = many' letter_iso8859_15
