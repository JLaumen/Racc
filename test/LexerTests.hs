{-# LANGUAGE OverloadedStrings #-}

module LexerTests (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Racc.Diagnostic (Diagnostic (Diagnostic), Located (Located), Pos (Pos), Span (Span))
import Racc.Lexer (Token (TDedent, TEnd, TIndent, TIndex, TInt, TKeyword, TLower, TNeg, TNewline, TSymbol, TUpper), tokenize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "lexer" $
    [testCase (show src) (tokens src @?= Right (ts <> [TNewline, TEnd])) | (src, ts) <- oneLine]
      <> [testCase (show src) (tokens src @?= Right ts) | (src, ts) <- files]
      <> [testCase (show src) (failure src @?= e) | (src, e) <- rejected]

-- | Sources of one logical line, without the final NEWLINE and END.
oneLine :: [(ByteString, [Token])]
oneLine =
  [ ("x x1 x' _x _ X Xy iff", [l "x", l "x1", l "x'", l "_x", s "_", u "X", u "Xy", l "iff"]),
    (BC.unwords keywords, map TKeyword keywords),
    ("0 007 1_000 123456789012345678901", map TInt [0, 7, 1000, 123456789012345678901]),
    (symbols, map s (BC.words symbols)),
    ("a->b<=c<<=d", [l "a", s "->", l "b", s "<=", l "c", s "<<", s "=", l "d"]),
    ("a--b", [l "a"]),
    -- INDEX after a name, `)`, and `]`; otherwise a list
    ("x[a] X[a] (x)[a] x[a][b]", [l "x", TIndex, l "a", s "]", u "X", TIndex, l "a", s "]", s "(", l "x", s ")", TIndex, l "a", s "]", l "x", TIndex, l "a", s "]", TIndex, l "b", s "]"]),
    ("x [a] 1[a]", [l "x", s "[", l "a", s "]", TInt 1, s "[", l "a", s "]"]),
    -- subtraction after every kind of operand
    ("a-1 A-1 1-1 (a)-1 [a]-1 {a}-1 result-1", concatMap (<> [s "-", TInt 1]) [[l "a"], [u "A"], [TInt 1], [s "(", l "a", s ")"], [s "[", l "a", s "]"], [s "{", l "a", s "}"], [k "result"]]),
    ("a - b- c -", [l "a", s "-", l "b", s "-", l "c", s "-"]),
    -- negation without a preceding operand, or with whitespace only before
    ("-a f -b (-c) * -d, if -e", [TNeg, l "a", l "f", TNeg, l "b", s "(", TNeg, l "c", s ")", s "*", TNeg, l "d", s ",", k "if", TNeg, l "e"])
  ]
  where
    symbols :: ByteString
    symbols = "-> => == != <= >= << >> ** .. <- = : | , ( ) [ ] { } \\ + * / % < > & ^ ~"
    keywords :: [ByteString]
    keywords =
      ["and", "as", "break", "by", "continue", "downto", "effects", "elif", "else", "ensures", "for"]
        <> ["from", "if", "in", "invariant", "match", "modifies", "not", "or", "otherwise", "pass"]
        <> ["requires", "result", "return", "state", "then", "to", "type", "until", "while", "yield"]

files :: [(ByteString, [Token])]
files =
  [ ("", [TEnd]),
    -- the example of GRAMMAR.md
    ( "f x =\n  if x < 0\n    0\n  else\n    x\n",
      [l "f", l "x", s "=", TNewline, TIndent, k "if", l "x", s "<", TInt 0, TNewline, TIndent, TInt 0, TNewline, TDedent, k "else", TNewline, TIndent, l "x", TNewline, TDedent, TDedent, TEnd]
    ),
    ("a\n  b\n    c\nd\n", [l "a", TNewline, TIndent, l "b", TNewline, TIndent, l "c", TNewline, TDedent, TDedent, l "d", TNewline, TEnd]),
    ("  a\n", [TIndent, l "a", TNewline, TDedent, TEnd]),
    -- blank, space-only, and comment lines; no final line break
    ("a\n\n   \n  -- c\nb", [l "a", TNewline, l "b", TNewline, TEnd]),
    ("a\r\n  b\r\n", [l "a", TNewline, TIndent, l "b", TNewline, TDedent, TEnd]),
    ("a -- \t\195\169\n", [l "a", TNewline, TEnd]),
    ("a\n-b\n", [l "a", TNewline, TNeg, l "b", TNewline, TEnd]),
    -- line breaks inside brackets, including INDEX
    ("x = (1,\n    x[\n0])\ny\n", [l "x", s "=", s "(", TInt 1, s ",", l "x", TIndex, TInt 0, s "]", s ")", TNewline, l "y", TNewline, TEnd]),
    (")\n  a\n", [s ")", TNewline, TIndent, l "a", TNewline, TDedent, TEnd])
  ]

-- | One input for each way the lexer can fail, with its error.
rejected :: [(ByteString, String)]
rejected =
  [ ("f x =\n\tx\n", "2:1: tab character; indent with spaces"),
    ("x = 1\ry = 2\n", "1:6: carriage return without a line feed"),
    ("x = caf\195\169\n", "1:8: non-ASCII character outside a comment"),
    ("!", "1:1: unexpected character `!`"),
    (".", "1:1: unexpected character `.`"),
    ("12abc", "1:1: invalid number literal `12abc`"),
    ("1__0", "1:1: invalid number literal `1__0`"),
    ("1_", "1:1: invalid number literal `1_`"),
    ("f x =\n    if x\n      1\n  2\n", "4:3: indentation does not match any enclosing block")
  ]

l, u, k, s :: ByteString -> Token
l = TLower
u = TUpper
k = TKeyword
s = TSymbol

failure :: ByteString -> String
failure = either (\(Diagnostic (Span (Pos ln c) _) m) -> show ln <> ":" <> show c <> ": " <> m) (const "accepted") . tokenize

tokens :: ByteString -> Either String [Token]
tokens = either (Left . show) (Right . map (\(Located _ t) -> t)) . tokenize
