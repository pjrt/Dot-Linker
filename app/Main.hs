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
matchAndLink dot = do
    h <- home
    let dotAsText = asText dot
        targetM = M.lookup dotAsText (fileMaps h)
    maybe (unmatchFile $ asText dot) matchedFile targetM
  where
    unmatchFile d = echo $ "No match found for dot " <> d <> ". Skipping..."
    matchedFile target = do
      targetExists <- testfile target
      let targetAsText = asText target
      if targetExists
        then echo $ asText dot <> " already exists. Skipping..."
        else do
          realdot <- realpath dot
          echo $ "Linking " <> asText realdot <> " -> " <> asText target
          sln realdot target

    asText = either id id . toText


fileMaps :: FilePath -> M.HashMap Text FilePath
fileMaps home =
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
    onHome f = home <> f


sln :: MonadIO io => FilePath -> FilePath -> io ()
sln src target = liftIO $ createSymbolicLink (toText' src) (toText' target)
  where toText' = unpack . either id id . toText
