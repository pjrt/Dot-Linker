{-# LANGUAGE OverloadedStrings #-}
module DotLinker
( lineMapParser
, fileMapParser
, matchAndLink
, expandPath
, Entry
) where

import Control.Applicative
import Control.Monad (forM_)
import Data.Attoparsec.ByteString.Char8
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Filesystem.Path.CurrentOS (FilePath, encode)
import System.Posix.Files (createSymbolicLink)
import Prelude hiding (takeWhile, FilePath)
import Turtle ((</>), (<>), MonadIO, fromText, toText, liftIO)
import qualified Turtle as T

import DotLinker.Parsers

type MappedDots = M.HashMap Text [FilePath]

-- | Given a map of dot files to their location and a dot file, make a symbolic
-- link for the given dotfile IFF a mapping is found.
--
-- NOTE: If the symbolic link already exists or there is no mapping in for the
-- given file, this does nothing.
matchAndLink :: MonadIO io => MappedDots -> FilePath -> io ()
matchAndLink mapped dotfile = do
    let dotfileAsText = asText . T.filename $ dotfile
        targetM = M.lookup dotfileAsText mapped
    maybe (unmatchFile $ asText dotfile) matchedFiles targetM
  where
    unmatchFile df = T.echo $ "No match found for dotfile " <> df <> ". Skipping..."
    matchedFiles targets = forM_ targets $ \target -> do
      targetExists <- (||) <$> T.testfile target <*> T.testdir target
      if targetExists
        then T.echo $ asText dotfile <> " already exists. Skipping..."
        else do
          realdotfile <- T.realpath dotfile
          ensurePathExist target
          T.echo $ "Linking " <> asText realdotfile <> " -> " <> asText target
          lns realdotfile target
      where
        ensurePathExist = T.mktree . T.directory

-- | Expand any enviroment variables found in the path
expandPath :: MonadIO io => FilePath -> io FilePath
expandPath path = do
    parts <- either (T.die . pack) (traverse expand) $ parseOnly envParser (encode path)
    return $ foldl' (\a x -> a </> fromText x) "/" parts
  where
    expand (Lit p) = return $ decodeUtf8 p
    expand (Env e) =
      let dieMsg = decodeUtf8 $ "Enviroment " <> e <> " not set"
      in maybe (T.die dieMsg) return =<< T.need (decodeUtf8 e)

-- | Create a symbolic link (ln -s)
lns :: MonadIO io => FilePath -> FilePath -> io ()
lns src target = liftIO $ createSymbolicLink (toText' src) (toText' target)
  where toText' = unpack . asText

-- Utils ----------------------------------------------------------------------
-- TODO:pjrt this is lazy and probably wrong
asText :: FilePath -> Text
asText = either id id . toText
