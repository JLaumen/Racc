module Main (main) where

import Data.ByteString qualified as BS
import Racc (compile, parseTree)
import Racc.Diagnostic (render)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (hIsTerminalDevice, stderr)

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
      either (\d -> colour >>= \c -> die (render c file src d)) putStr (f src)
    -- On a terminal that shows colour, unless NO_COLOR is set (no-color.org).
    colour = do
      tty <- hIsTerminalDevice stderr
      term <- lookupEnv "TERM"
      noColour <- lookupEnv "NO_COLOR"
      pure (tty && term /= Just "dumb" && maybe True null noColour)
