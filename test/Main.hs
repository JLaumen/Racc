-- | Runs the unit tests and two kinds of golden tests:
--
-- * test/run/X.racc is compiled, run, and its output compared with X.out.
-- * test/errors/X.racc must fail to compile with the message in X.err.
module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import LexerTests qualified
import Racc (compile)
import Racc.Diagnostic (render)
import System.FilePath (replaceExtension, takeBaseName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = do
  runs <- findByExtension [".racc"] "test/run"
  errors <- findByExtension [".racc"] "test/errors"
  defaultMain $
    testGroup
      "racc"
      [ LexerTests.tests,
        testGroup "run" (map (golden ".out" run) runs),
        testGroup "errors" (map (golden ".err" failure) errors)
      ]

golden :: String -> (FilePath -> IO String) -> FilePath -> TestTree
golden ext test file =
  goldenVsString (takeBaseName file) (replaceExtension file ext) (LBS.pack <$> test file)

run :: FilePath -> IO String
run file = do
  src <- BS.readFile file
  c <- either (fail . render file src) pure (compile src)
  withSystemTempDirectory "racc" $ \dir -> do
    let exe = dir </> "a.out"
    _ <- readProcess "cc" ["-x", "c", "-o", exe, "-"] c
    readProcess exe [] ""

failure :: FilePath -> IO String
failure file = do
  src <- BS.readFile file
  either (pure . (<> "\n") . render file src) (const (fail "compiled without error")) (compile src)
