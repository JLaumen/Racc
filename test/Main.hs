{-# LANGUAGE OverloadedStrings #-}

-- | Runs the unit tests and three kinds of golden tests:
--
-- * test/parse/X.racc is parsed and its tree compared with X.tree; so is the
--   example of README.md, with README.tree.
-- * test/run/X.racc is compiled, run, and its output compared with X.out.
-- * test/errors/X.racc must fail to compile with the error in X.err.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as LBS
import LexerTests qualified
import ParserTests qualified
import Racc (compile, parseTree)
import Racc.Diagnostic (render)
import System.FilePath (replaceExtension, takeBaseName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = do
  parses <- findByExtension [".racc"] "test/parse"
  runs <- findByExtension [".racc"] "test/run"
  errors <- findByExtension [".racc"] "test/errors"
  readme <- example <$> BS.readFile "README.md"
  seeds <- (readme :) <$> traverse BS.readFile (parses <> runs <> errors)
  defaultMain $
    testGroup
      "racc"
      [ LexerTests.tests,
        ParserTests.tests seeds,
        testGroup "parse" $
          goldenVsString "README" "test/parse/README.tree" (LBS.pack <$> tree "README.md" readme)
            : map (golden ".tree" tree) parses,
        testGroup "run" (map (golden ".out" run) runs),
        testGroup "errors" (map (golden ".err" failure) errors)
      ]

-- | The first code block of a Markdown file.
example :: ByteString -> ByteString
example = BC.unlines . takeWhile (/= "```") . drop 1 . dropWhile (/= "```") . BC.lines

golden :: String -> (FilePath -> ByteString -> IO String) -> FilePath -> TestTree
golden ext test file =
  goldenVsString (takeBaseName file) (replaceExtension file ext) (LBS.pack <$> (test file =<< BS.readFile file))

run :: FilePath -> ByteString -> IO String
run file src = do
  c <- either (fail . render file src) pure (compile src)
  withSystemTempDirectory "racc" $ \dir -> do
    let exe = dir </> "a.out"
    _ <- readProcess "cc" ["-x", "c", "-o", exe, "-"] c
    readProcess exe [] ""

tree :: FilePath -> ByteString -> IO String
tree file src = either (fail . render file src) pure (parseTree src)

failure :: FilePath -> ByteString -> IO String
failure file src = either (pure . (<> "\n") . render file src) (const (fail "compiled without error")) (compile src)
