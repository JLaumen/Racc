module Main (main) where

import Data.ByteString qualified as BS
import Racc (compile)
import Racc.Diagnostic (render)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- BS.readFile file
      either (die . render file src) putStr (compile src)
    _ -> die "usage: racc FILE.racc"
