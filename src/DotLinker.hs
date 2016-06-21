{-# LANGUAGE OverloadedStrings #-}
module DotLinker
( lineMapParser
, fileMapParser
, matchAndLink
, Entry(..)
) where

import Control.Applicative
import Control.Monad (forM_)
import Data.Attoparsec.ByteString.Char8
import qualified Data.HashMap.Strict as M
import Data.Text (Text, pack, unpack)
import Filesystem.Path.CurrentOS (FilePath, decode)
import System.Posix.Files (createSymbolicLink)
import Prelude hiding (takeWhile, FilePath)
import Turtle ((<>), MonadIO, toText, liftIO)
import Turtle.Prelude

data Entry = Entry Text [FilePath]
  deriving (Eq, Show)

-- | Parser for a single line in the file
--
-- Example:
--
-- vimrc: /home/rar/.vimrc,/home/rar/.config/nvim/init.vim
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

-- | Parser for a whole file
fileMapParser :: Parser [Entry]
fileMapParser = lineMapParser `manyTill` endOfInput

type MappedDots = M.HashMap Text [FilePath]

-- | Given a map of dot files to their location and a dot file, make a symbolic
-- link for the given dotfile IFF a mapping is found.
--
-- NOTE: If the symbolic link already exists or there is no mapping in for the
-- given file, this does nothing.
matchAndLink :: MonadIO io => MappedDots -> FilePath -> io ()
matchAndLink mapped dotfile = do
    let dotfileAsText = asText dotfile
        targetM = M.lookup dotfileAsText mapped
    maybe (unmatchFile $ asText dotfile) matchedFiles targetM
  where
    unmatchFile df = echo $ "No match found for dotfile " <> df <> ". Skipping..."
    matchedFiles targets = forM_ targets $ \target -> do
      targetExists <- testfile target
      if targetExists
        then echo $ asText dotfile <> " already exists. Skipping..."
        else do
          realdotfile <- realpath dotfile
          echo $ "Linking " <> asText realdotfile <> " -> " <> asText target
          lns realdotfile target

    -- TODO:pjrt this is lazy and probably wrong
    asText = either id id . toText


-- | Create a symbolic link (ln -s)
lns :: MonadIO io => FilePath -> FilePath -> io ()
lns src target = liftIO $ createSymbolicLink (toText' src) (toText' target)
  where toText' = unpack . either id id . toText
