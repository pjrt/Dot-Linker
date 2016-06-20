{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (unpack)
import Turtle
import qualified Data.HashMap.Strict as M
import Prelude hiding (FilePath)
import System.Posix.Files (createSymbolicLink)

main :: IO ()
main = sh $ do
    dots <- options "Dot-Linker" parsePath
    cd dots
    file <- filename <$> ls "./"
    matchAndLink file
  where
    parsePath = argPath "dots location" ""


matchAndLink :: FilePath -> Shell ()
matchAndLink dotfile = do
    h <- home
    let dotfileAsText = asText dotfile
        targetM = M.lookup dotfileAsText (fileMaps h)
    maybe (unmatchFile $ asText dotfile) matchedFile targetM
  where
    unmatchFile df = echo $ "No match found for dotfile " <> df <> ". Skipping..."
    matchedFile target = do
      targetExists <- testfile target
      if targetExists
        then echo $ asText dotfile <> " already exists. Skipping..."
        else do
          realdotfile <- realpath dotfile
          echo $ "Linking " <> asText realdotfile <> " -> " <> asText target
          sln realdotfile target

    asText = either id id . toText


fileMaps :: FilePath -> M.HashMap Text FilePath
fileMaps h =
  M.fromList
    [ ("ctags",        onHome ".ctags-2")
    , ("gitconfig",    onHome ".gitconfig")
    , ("git_template", onHome ".git_template")
    , ("vimrc",        onHome ".vimrc")
    , ("xmobarrc",     onHome ".xmobarrc")
    , ("xmonad.hs",    onHome ".xmonad/xmonad.hs")
    , ("Xdefaults",    onHome ".Xdefaults")
    , ("zshrc",        onHome ".zshrc")
    ]
  where
    onHome = (<>) h


sln :: MonadIO io => FilePath -> FilePath -> io ()
sln src target = liftIO $ createSymbolicLink (toText' src) (toText' target)
  where toText' = unpack . either id id . toText
