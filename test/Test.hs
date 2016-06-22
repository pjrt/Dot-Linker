{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString
import Data.Monoid
import Data.ByteString.Char8
import DotLinker
import Turtle (runManaged, pwd, mktempdir, (</>), liftIO, toText, testfile, touch)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as M

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "file parser"
          [ lineparserTest, fileparserTests, ioTests]

lineparserTest :: TestTree
lineparserTest =
  testCase "should parse a line fine" $
    let x = parseOnly lineMapParser "vim_rc:  /root/har/.vimrc, /root/har/.config/nvim/init.vim"
        expected = Right $ Entry "vim_rc" ["/root/har/.vimrc", "/root/har/.config/nvim/init.vim"]
    in x @?= expected


fileparserTests :: TestTree
fileparserTests =
  testGroup "File parser tests"
    [fileparserTest "\n", fileparserTest "\r", fileparserTest "\n\r"]

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


ioTests :: TestTree
ioTests = testGroup "IO tests" [dirMakeTest]


dirMakeTest :: TestTree
dirMakeTest =
  testCase "in `/a/b/c.link`, if `b` doesn't exist, it should be made" $
    runManaged $
      do parent <- pwd
         testPath <- mktempdir parent "test-dir"
         let dotFile = "something.link"
             dotFilePath = testPath </> dotFile
             expectedFile = testPath </> "non-existent-dir" </> "ned" </> dotFile
             mappedDots = M.fromList [(toText' dotFile, [expectedFile])]
         touch dotFilePath
         matchAndLink mappedDots dotFilePath
         liftIO $ assertBool (show expectedFile ++ " does not exist")
                              =<< testfile expectedFile
    where
      toText' = either id id . toText
