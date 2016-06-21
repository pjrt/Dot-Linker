{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Monad (forM_)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import DotLinker
import Turtle
import qualified Data.HashMap.Strict as M
import Prelude hiding (FilePath)
import System.Posix.Files (createSymbolicLink)

type MappedDots = M.HashMap Text [FilePath]

main :: IO ()
main = sh $ do
    (dots, mapFileLoc) <- options "dot-linker" parseOpts
    mapFile <- liftIO $ encodeUtf8 <$> readTextFile mapFileLoc
    let entriesE = toHashMap <$> parseOnly fileMapParser mapFile
    parsedMap <- either (die . pack) return entriesE
    echo $ pack . show $ parsedMap
    cd dots
    file <- filename <$> ls "./"
    matchAndLink parsedMap file
  where
    parseOpts = (,) <$> parseDotsPath <*> parseMapPath
    parseDotsPath = argPath "<dot-files location>" "Path to where the dot files are located"
    parseMapPath = argPath "<parse-map location>" "Path to where the mapping file is located"

    toHashMap = M.fromList . fmap toTups
      where toTups (Entry k v) = (k, v)


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
          sln realdotfile target

    asText = either id id . toText


sln :: MonadIO io => FilePath -> FilePath -> io ()
sln src target = liftIO $ createSymbolicLink (toText' src) (toText' target)
  where toText' = unpack . either id id . toText
