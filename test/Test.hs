{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString
import Data.Monoid
import Data.ByteString.Char8
import DotLinker
import Turtle ((</>), liftIO)
import qualified Turtle as T
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as M

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "file parser"
          [ lineparserTest, commentedLineTest, fileparserTests, ioTests]
entry :: a -> b -> (a, b)
entry = (,)

lineparserTest :: TestTree
lineparserTest =
  testCase "should parse a line fine" $
    let x = parseOnly lineMapParser "vim_rc:  /root/har/.vimrc, /root/har/.config/nvim/init.vim"
        expected = Right $ entry "vim_rc" ["/root/har/.vimrc", "/root/har/.config/nvim/init.vim"]
    in x @?= expected

commentedLineTest :: TestTree
commentedLineTest =
  testCase "should not parse a commented line" $
    let x = parseOnly fileMapParser
              $ "vimrc:  /root/har/.vimrc, /root/har/.config/nvim/init.vim\n" <>
                "-- ctags: /root/har/.ctags\n" <>
                "zshrc: /root/har/.zshrc\n"
        expected = Right
                     [ entry "vimrc" ["/root/har/.vimrc", "/root/har/.config/nvim/init.vim"]
                     , entry "zshrc" ["/root/har/.zshrc"]
                     ]
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
                     [ entry "vimrc" ["/root/har/.vimrc", "/root/har/.config/nvim/init.vim"]
                     , entry "zshrc" ["/root/har/.zshrc"]
                     ]
    in x @?= expected


ioTests :: TestTree
ioTests = testGroup "IO tests" [dirMakeTest, envParserTest]

mkTestDir :: T.Managed (M.HashMap T.Text [T.FilePath], T.FilePath, T.FilePath)
mkTestDir =
 do parent <- T.pwd
    testPath <- T.mktempdir parent "test-dir"
    let dotFile = "something.link"
        dotFilePath = testPath </> dotFile
        expectedFile = testPath </> "non-existent-dir" </> "ned" </> dotFile
        mappedDots = M.fromList [(toText' dotFile, [expectedFile])]
    T.touch dotFilePath
    return (mappedDots, dotFilePath, expectedFile)


dirMakeTest :: TestTree
dirMakeTest =
  testCase "in `/a/b/c.link`, if `b` doesn't exist, it should be made" $
    T.runManaged $
      do (mappedDots, dotFilePath, expectedFile) <- mkTestDir
         matchAndLink (False, False) mappedDots dotFilePath
         liftIO $ assertBool (show expectedFile ++ " does not exist")
                              =<< T.testfile expectedFile

dryRunTest :: TestTree
dryRunTest =
  testCase "dry run should not link anything" $
    T.runManaged $
      do (mappedDots, dotFilePath, expectedFile) <- mkTestDir
         matchAndLink (False, True) mappedDots dotFilePath
         liftIO $ assertBool (show expectedFile ++ " does exist") . not
                              =<< T.testfile expectedFile

envParserTest :: TestTree
envParserTest = testGroup "Enviroment Parsing" [midPath, beginning, multi]
  where
    midPath =
      testCase "envParser should parse env variables mid path"
        $ do let exportedPath = "/home/har"
             T.export "TEST_HOME" (toText' exportedPath)
             expanedPath <- expandPath "/root/$TEST_HOME/.dort/lol"
             let expectedPath = "/root" </> exportedPath </> ".dort/lol"
             expanedPath @?= expectedPath
    beginning =
      testCase "envParser should parse env variables at the beginning"
        $ do let exportedPath = "/home/har"
             T.export "TEST_HOME" (toText' exportedPath)
             expanedPath <- expandPath "$TEST_HOME/.dort/lol"
             let expectedPath = exportedPath </> ".dort/lol"
             expanedPath @?= expectedPath
    multi =
      testCase "envParser should parse env multiple variables"
        $ do let exportedPath1 = "/home/har"
                 exportedPath2 = "hor/"
             T.export "TEST_HOME" (toText' exportedPath1)
             T.export "TEST_dir" (toText' exportedPath2)
             expanedPath <- expandPath "$TEST_HOME/.dort/$TEST_dir/har"
             let expectedPath = exportedPath1 </> ".dort" </> exportedPath2 </> "har"
             expanedPath @?= expectedPath

toText' :: T.FilePath -> T.Text
toText' = either id id . T.toText
