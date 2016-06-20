{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString (parseOnly)
import DotLinker
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "file parser" [fileparserTest]

fileparserTest :: TestTree
fileparserTest =
  testCase "should parse a line fine" $
    let x = parseOnly fileMapParser "vimrc:  /root/har/.vimrc, /root/har/.config/nvim/init.vim"
        expected = Right $ Entry "vimrc" ["/root/har/.vimrc", "/root/har/.config/nvim/init.vim"]
    in x @?= expected
