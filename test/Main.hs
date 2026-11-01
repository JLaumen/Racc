{-# LANGUAGE OverloadedStrings #-}

-- | Runs the unit tests and five kinds of golden tests:
--
-- * test/parse/X.racc is parsed and its tree compared with X.tree.
-- * test/resolve/X.racc is resolved, and its warnings and tree compared with
--   X.tree; so is the example of README.md, with README.tree.
-- * test/types/X.racc is type checked, and its warnings and types compared
--   with X.types; so is the example of README.md, with README.types.
-- * test/run/X.racc is compiled, run, and its output compared with X.out.
-- * test/errors/X.racc must fail to compile with the error in X.err; one of
--   them also in colour, in X.colour.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as LBS
import LexerTests qualified
import ParserTests qualified
import Racc (compile, parseTree, resolveTree, types)
import Racc.Diagnostic (Result, render)
import System.FilePath (replaceExtension, takeBaseName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = do
  parses <- findByExtension [".racc"] "test/parse"
  resolves <- findByExtension [".racc"] "test/resolve"
  typeds <- findByExtension [".racc"] "test/types"
  runs <- findByExtension [".racc"] "test/run"
  errors <- findByExtension [".racc"] "test/errors"
  readme <- example <$> BS.readFile "README.md"
  seeds <- (readme :) <$> traverse BS.readFile (parses <> resolves <> typeds <> runs <> errors)
  defaultMain $
    testGroup
      "racc"
      [ LexerTests.tests,
        ParserTests.tests seeds,
        testGroup "parse" (map (golden ".tree" (success parseTree)) parses),
        testGroup "resolve" $
          goldenVsString "README" "test/resolve/README.tree" (LBS.pack <$> success resolveTree "README.md" readme)
            : map (golden ".tree" (success resolveTree)) resolves,
        testGroup "types" $
          goldenVsString "README" "test/types/README.types" (LBS.pack <$> success types "README.md" readme)
            : map (golden ".types" (success types)) typeds,
        testGroup "run" (map (golden ".out" run) runs),
        testGroup "errors" $
          goldenVsString "colour" (replaceExtension colour ".colour") (LBS.pack <$> (failure True colour =<< BS.readFile colour))
            : map (golden ".err" (failure False)) errors
      ]

colour :: FilePath
colour = "test/errors/tuple-close.racc"

-- | The first code block of a Markdown file.
example :: ByteString -> ByteString
example = BC.unlines . takeWhile (/= "```") . drop 1 . dropWhile (/= "```") . BC.lines

golden :: String -> (FilePath -> ByteString -> IO String) -> FilePath -> TestTree
golden ext test file =
  goldenVsString (takeBaseName file) (replaceExtension file ext) (LBS.pack <$> (test file =<< BS.readFile file))

run :: FilePath -> ByteString -> IO String
run file src = do
  let (ds, out) = compile src
  c <- maybe (fail (render False file src ds)) pure out
  withSystemTempDirectory "racc" $ \dir -> do
    let exe = dir </> "a.out"
    _ <- readProcess "cc" ["-x", "c", "-o", exe, "-"] c
    readProcess exe [] ""

-- | The output of a phase that must succeed, after its warnings.
success :: (ByteString -> Result String) -> FilePath -> ByteString -> IO String
success phase file src = case phase src of
  (ds, Just out) -> pure (render False file src ds <> out)
  (ds, Nothing) -> fail (render False file src ds)

failure :: Bool -> FilePath -> ByteString -> IO String
failure c file src = case compile src of
  (ds, Nothing) -> pure (render c file src ds)
  _ -> fail "compiled without error"
