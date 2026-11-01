{-# LANGUAGE RoleAnnotations #-}

module Racc.Diagnostic (Pos (..), Span (..), Located (..), Severity (..), Diagnostic (..), Result, render, quote) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.List (intercalate, intersperse)

-- | Line and column, both counted from 1; columns count bytes.
data Pos = Pos {line :: !Int, col :: !Int}
  deriving stock (Eq, Ord, Show)

-- | From the start (inclusive) to the end (exclusive).
data Span = Span !Pos !Pos
  deriving stock (Eq, Ord, Show)

-- | A value with its place in the source.
type role Located representational

data Located a = Located !Span !a
  deriving stock (Functor, Foldable, Traversable)

data Severity = Error | Warning
  deriving stock (Eq, Show)

data Diagnostic = Diagnostic !Severity !Span String
  deriving stock (Show)

-- | Diagnostics, and the result if none is an error.
type Result a = ([Diagnostic], Maybe a)

-- | Each diagnostic as its location, message, and the source line with the
-- span underlined, separated by blank lines; in colour (ANSI escape codes)
-- if the flag is set.
render :: Bool -> FilePath -> ByteString -> [Diagnostic] -> String
render colour path src = unlines . intersperse "" . map one
  where
    style code x = if colour then "\ESC[" <> code <> "m" <> x <> "\ESC[0m" else x
    bold = style "1"
    blue = style "1;34"
    one (Diagnostic severity (Span s e) msg) =
      intercalate
        "\n"
        [ bold (path <> ":" <> show (line s) <> ":" <> show (col s) <> ":") <> " " <> mark label <> " " <> bold msg,
          blue gutter,
          blue (num <> " |") <> " " <> text,
          blue gutter <> " " <> replicate (col s - 1) ' ' <> mark (replicate width '^')
        ]
      where
        (label, mark) = case severity of
          Error -> ("error:", style "1;31")
          Warning -> ("warning:", style "1;35")
        num = show (line s)
        gutter = replicate (length num) ' ' <> " |"
        text = concatMap (takeWhile (/= '\r') . BC.unpack) (take 1 (drop (line s - 1) (BC.lines src)))
        width
          | line e == line s = max 1 (col e - col s)
          | otherwise = max 1 (length text - col s + 1)

-- | A name in a message, such as @`x`@.
quote :: ByteString -> String
quote b = "`" <> BC.unpack b <> "`"
