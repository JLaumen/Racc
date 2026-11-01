module Main (main) where

import Data.ByteString qualified as BS
import Racc (compile, parseTree)
import Racc.Diagnostic (render)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parse", file] -> output parseTree file
    [file@(c : _)] | c /= '-', file /= "parse" -> output compile file
    _ -> die "usage: racc [parse] FILE.racc"
  where
    output f file = do
      src <- BS.readFile file
      either (die . render file src) putStr (f src)
