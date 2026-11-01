module Main (main) where

import Racc (compile)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= either die putStr . compile
    _ -> die "usage: racc FILE.racc"
