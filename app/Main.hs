{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Attoparsec.ByteString (parseOnly)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import qualified Data.HashMap.Strict as M
import DotLinker
import Turtle
import Prelude hiding (FilePath)

main :: IO ()
main = sh $ do
    (dots, mapFileLoc, dryRun) <- options "dot-linker" parseOpts
    mapFile <- liftIO $ encodeUtf8 <$> readTextFile mapFileLoc
    let entriesE = toHashMap <$> parseOnly fileMapParser mapFile
    parsedMap <- either (die . pack) return entriesE
    expandedMap <- traverse (sequenceA . map expandPath) parsedMap
    cd dots
    file <- filename <$> ls "./"
    matchAndLink dryRun expandedMap file
  where
    parseOpts = (,,) <$> parseDotsPath <*> parseMapPath <*> parseDry
    parseDotsPath = argPath "<dots-location>" "Path to where the dot files are located"
    parseMapPath = argPath "<mappings-location>" "Path to where the mapping file is located"
    parseDry = switch "dry-run" 'd' "Do not link anything, simply show what would happen"

    toHashMap = M.fromList
