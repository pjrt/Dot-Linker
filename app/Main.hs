{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.HashMap.Strict as M

main :: IO ()
main = sh $ do
  curr <- pwd
  file <- basename <$> find (suffix "dot") curr
  matchAndMove file


matchAndMove :: FilePath -> Shell ()
matchAndMove file = do
    h <- home
    Just l <- M.map file (fileMaps h)
    inproc "ln" ["-s", asText file, asText l] empty
  where
    asText = either id id . toText


fileMaps :: FilePath -> HashMap FilePath FilePath
fileMaps home = M.fromList [
             ("ctags",        fromHome ".ctags")
           , ("gitconfigs",   fromHome ".gitconfig")
           , ("git_template", fromHome ".git_template")
           , ("vimrc",        fromHome ".vimrc")
           , ("xmobarrc",     fromHome ".xmobarrc")
           , ("xmonad.hs",    fromHome ".xmonad/xmonad.hs")
           , ("Xdefaults",    fromHome ".Xdefaults")
           , ("zshrc",        fromHome ".zshrc")
           ]
  where
    fromHome f = home <> f
