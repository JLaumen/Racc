module Main (main) where

import Data.ByteString qualified as BS
import Racc (compile, parseTree, resolveTree)
import Racc.Diagnostic (render)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (hIsTerminalDevice, hPutStr, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parse", file] -> output parseTree file
    ["resolve", file] -> output resolveTree file
    [file@(c : _)] | c /= '-', file `notElem` ["parse", "resolve"] -> output compile file
    _ -> die "usage: racc [parse | resolve] FILE.racc"
  where
    output f file = do
      src <- BS.readFile file
      let (ds, result) = f src
      c <- colour
      hPutStr stderr (render c file src ds)
      maybe exitFailure putStr result
    -- On a terminal that shows colour, unless NO_COLOR is set (no-color.org).
    colour = do
      tty <- hIsTerminalDevice stderr
      term <- lookupEnv "TERM"
      noColour <- lookupEnv "NO_COLOR"
      pure (tty && term /= Just "dumb" && maybe True null noColour)
