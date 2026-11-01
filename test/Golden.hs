-- | Each test/golden/X.racc is compiled, run, and its output compared with
-- test/golden/X.out.
module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as LBS
import Racc (compile)
import System.FilePath (replaceExtension, takeBaseName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = do
  files <- findByExtension [".racc"] "test/golden"
  defaultMain (testGroup "golden" (map golden files))

golden :: FilePath -> TestTree
golden file =
  goldenVsString (takeBaseName file) (replaceExtension file ".out") (run file)

run :: FilePath -> IO LBS.ByteString
run file = do
  c <- either fail pure . compile =<< readFile file
  withSystemTempDirectory "racc" $ \dir -> do
    let exe = dir </> "a.out"
    _ <- readProcess "cc" ["-x", "c", "-o", exe, "-"] c
    LBS.pack <$> readProcess exe [] ""
