module Main (main) where

import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Racc (compile, parseTree, resolveTree, types)
import Racc.Diagnostic (render)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (hIsTerminalDevice, hPutStr, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cmd, file] | Just f <- lookup cmd commands -> output f file
    [file@(c : _)] | c /= '-', isNothing (lookup file commands) -> output compile file
    _ -> die ("usage: racc [" <> intercalate " | " (map fst commands) <> "] FILE.racc")
  where
    commands = [("parse", parseTree), ("resolve", resolveTree), ("types", types)]
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
