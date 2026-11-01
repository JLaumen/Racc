{-# LANGUAGE RoleAnnotations #-}

module Racc.Diagnostic (Pos (..), Span (..), Located (..), Diagnostic (..), render) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.List (intercalate)

-- | Line and column, both counted from 1; columns count bytes.
data Pos = Pos {line :: !Int, col :: !Int}
  deriving stock (Eq, Show)

-- | From the start (inclusive) to the end (exclusive).
data Span = Span !Pos !Pos
  deriving stock (Eq, Show)

-- | A value with its place in the source.
type role Located representational

data Located a = Located !Span !a

data Diagnostic = Diagnostic !Span String
  deriving stock (Show)

-- | Location, message, and the source line with the span underlined; in
-- colour (ANSI escape codes) if the flag is set.
render :: Bool -> FilePath -> ByteString -> Diagnostic -> String
render colour path src (Diagnostic (Span s e) msg) =
  intercalate
    "\n"
    [ bold (path <> ":" <> show (line s) <> ":" <> show (col s) <> ":") <> " " <> red "error:" <> " " <> bold msg,
      blue gutter,
      blue (num <> " |") <> " " <> text,
      blue gutter <> " " <> replicate (col s - 1) ' ' <> red (replicate width '^')
    ]
  where
    style code x = if colour then "\ESC[" <> code <> "m" <> x <> "\ESC[0m" else x
    bold = style "1"
    red = style "1;31"
    blue = style "1;34"
    num = show (line s)
    gutter = replicate (length num) ' ' <> " |"
    text = concatMap (takeWhile (/= '\r') . BC.unpack) (take 1 (drop (line s - 1) (BC.lines src)))
    width
      | line e == line s = max 1 (col e - col s)
      | otherwise = max 1 (length text - col s + 1)
