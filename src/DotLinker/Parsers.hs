{-# LANGUAGE OverloadedStrings #-}
module DotLinker.Parsers
( lineMapParser
, envParser
, fileMapParser
, Entry
, EnvOrLit(..)
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Filesystem.Path.CurrentOS (FilePath, decode)
import Prelude hiding (takeWhile, FilePath)

type Entry = (Text, [FilePath])

-- | Parser for a single line in the file
--
-- Example:
--
-- vimrc: /home/rar/.vimrc,/home/rar/.config/nvim/init.vim
lineMapParser :: Parser Entry
lineMapParser = do
    src <- dotName <* char ':'
    ts <- filePath `sepBy1` char ','
    return $ (decodeUtf8 src, ts)
  where
    dotName = takeTill (== ':')
    filePath =
      strip $ decode <$> takeTill (inClass ",\n\r")

    strip str = many' space *> str <* many' space

-- | Parser for a whole file
fileMapParser :: Parser [Entry]
fileMapParser = lineMapParser `manyTill` endOfInput


data EnvOrLit = Env ByteString -- ^ Represents an enviroment variable
              | Lit ByteString -- ^ A literal path part

-- | Parser that parses a path with variables into parts
--
-- Example: $HOME/.dotfiles -> [Env HOME, Lit .dotfiles]
--
-- This will later be used by @expandPath@ to create:
--
-- [Env Home, Lit .dotfiles] -> /home/me/.dotfiles
envParser :: Parser [EnvOrLit]
envParser = secParser `sepBy1` char '/'
  where
    secParser =
      let env = char '$' *> takeWhile (inClass "a-zA-Z_") <?> "env variable"
          lit = takeTill (== '/') <?> "literal path part"
      in  (Env <$> env) <|> (Lit <$> lit)
