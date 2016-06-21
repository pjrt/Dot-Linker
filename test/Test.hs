{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString
import Data.Monoid
import Data.ByteString.Char8
import DotLinker
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "file parser"
          [ lineparserTest
          , fileparserTest "\n", fileparserTest "\r", fileparserTest "\n\r"]

lineparserTest :: TestTree
lineparserTest =
  testCase "should parse a line fine" $
    let x = parseOnly lineMapParser "vimrc:  /root/har/.vimrc, /root/har/.config/nvim/init.vim"
        expected = Right $ Entry "vimrc" ["/root/har/.vimrc", "/root/har/.config/nvim/init.vim"]
    in x @?= expected

fileparserTest :: ByteString -> TestTree
fileparserTest endLineChar =
  testCase ("should parse multiple lines fine with " <> show endLineChar) $
    let x = parseOnly fileMapParser
              $ "vimrc:  /root/har/.vimrc, /root/har/.config/nvim/init.vim"
              <> endLineChar <> "zshrc: /root/har/.zshrc" <> endLineChar
        expected = Right
                     [ Entry "vimrc" ["/root/har/.vimrc", "/root/har/.config/nvim/init.vim"]
                     , Entry "zshrc" ["/root/har/.zshrc"]
                     ]
    in x @?= expected
