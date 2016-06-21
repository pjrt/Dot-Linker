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
    parseDotsPath = argPath "<dots-location>" "Path to where the dot files are located"
    parseMapPath = argPath "<mappings-location>" "Path to where the mapping file is located"

    toHashMap = M.fromList . fmap toTups
      where toTups (Entry k v) = (k, v)
