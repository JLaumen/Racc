{-# LANGUAGE OverloadedStrings #-}

module LexerTests (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Either (isLeft)
import Racc.Lexer (Located (Located), Token (TDedent, TEnd, TIndent, TIndex, TInt, TKeyword, TLower, TNeg, TNewline, TSymbol, TUpper), tokenize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | Error messages are tested by the golden files in test/errors.
tests :: TestTree
tests =
  testGroup "lexer" $
    [testCase (show src) (tokens src @?= Right (ts <> [TNewline, TEnd])) | (src, ts) <- oneLine]
      <> [testCase (show src) (tokens src @?= Right ts) | (src, ts) <- files]
      <> [testCase (show src) (assertBool "accepted" (isLeft (tokenize src))) | src <- rejected]

-- | Sources of one logical line, without the final NEWLINE and END.
oneLine :: [(ByteString, [Token])]
oneLine =
  [ ("x x1 x' _x _ X Xy iff", [l "x", l "x1", l "x'", l "_x", s "_", u "X", u "Xy", l "iff"]),
    (BC.unwords keywords, map TKeyword keywords),
    ("0 007 1_000 123456789012345678901", map TInt [0, 7, 1000, 123456789012345678901]),
    (symbols, map s (BC.words symbols)),
    ("a->b<=c<<=d", [l "a", s "->", l "b", s "<=", l "c", s "<<", s "=", l "d"]),
    ("a -- b", [l "a"]),
    ("a--b", [l "a"]),
    -- INDEX after a name, `)`, and `]`; otherwise a list
    ("x[a] X[a] (x)[a] x[a][b]", [l "x", TIndex, l "a", s "]", u "X", TIndex, l "a", s "]", s "(", l "x", s ")", TIndex, l "a", s "]", l "x", TIndex, l "a", s "]", TIndex, l "b", s "]"]),
    ("x [a] 1[a]", [l "x", s "[", l "a", s "]", TInt 1, s "[", l "a", s "]"]),
    -- subtraction after every kind of operand
    ("a-1 A-1 1-1 (a)-1 [a]-1 {a}-1 result-1", concatMap (<> [s "-", TInt 1]) [[l "a"], [u "A"], [TInt 1], [s "(", l "a", s ")"], [s "[", l "a", s "]"], [s "{", l "a", s "}"], [k "result"]]),
    ("a - b", [l "a", s "-", l "b"]),
    ("a- b", [l "a", s "-", l "b"]),
    ("a -", [l "a", s "-"]),
    -- negation without a preceding operand, or with whitespace only before
    ("-a", [TNeg, l "a"]),
    ("f (-a) * -b, if -c", [l "f", s "(", TNeg, l "a", s ")", s "*", TNeg, l "b", s ",", k "if", TNeg, l "c"]),
    ("f -a", [l "f", TNeg, l "a"])
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
    ("-- c\n", [TEnd]),
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
    ("x = (1,\n2,\n    3)\ny\n", [l "x", s "=", s "(", TInt 1, s ",", TInt 2, s ",", TInt 3, s ")", TNewline, l "y", TNewline, TEnd]),
    ("x[\n0]\ny\n", [l "x", TIndex, TInt 0, s "]", TNewline, l "y", TNewline, TEnd]),
    (")\n  a\n", [s ")", TNewline, TIndent, l "a", TNewline, TDedent, TEnd])
  ]

-- | Errors whose message a golden file already covers for another input.
rejected :: [ByteString]
rejected = ["1_", "1'", "!", "."]

l, u, k, s :: ByteString -> Token
l = TLower
u = TUpper
k = TKeyword
s = TSymbol

tokens :: ByteString -> Either String [Token]
tokens = either (Left . show) (Right . map (\(Located _ t) -> t)) . tokenize
