{-# LANGUAGE OverloadedStrings #-}

-- | What the golden files can't show: spans, and that parsing never crashes.
module ParserTests (tests) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (toList)
import LexerTests (keywords, symbols)
import Racc (parseTree)
import Racc.Diagnostic (Located (Located), Pos (Pos), Span (Span))
import Racc.Lexer (tokenize)
import Racc.Parser (parse)
import Racc.Syntax (Block (Block), Body (BlockBody), Decl (EquationDecl), Equation (Equation), Rhs (Plain))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, Property, choose, elements, forAll, testProperty, withNumTests, within)

-- | The seeds are the sources of the golden tests.
tests :: [ByteString] -> TestTree
tests seeds = testGroup "parser" [testCase "spans" spans, testProperty "never crashes" (neverCrashes seeds)]

-- | Spans cover the node's tokens, without the line break or the next line.
spans :: IO ()
spans = do
  let decls = either (error . show) id (parse =<< tokenize "f =\n  if a\n    b\n  x + y\n")
  case decls of
    [Located d (EquationDecl (Equation _ _ (Plain (BlockBody (Block _ ss)))))] -> do
      d @?= Span (Pos 1 1) (Pos 4 8)
      map (\(Located sp _) -> sp) (toList ss) @?= [Span (Pos 2 3) (Pos 3 6), Span (Pos 4 3) (Pos 4 8)]
    _ -> assertBool "unexpected tree" False

-- | Lexing and parsing end, without an exception, on the seeds with a few
-- random edits, so that most inputs get deep into the parser.
neverCrashes :: [ByteString] -> Property
neverCrashes seeds = withNumTests 10000 $ forAll mutant $ \src -> within 1000000 (length (show (parseTree src)) `seq` True)
  where
    mutant :: Gen ByteString
    mutant = do
      src <- elements seeds
      n <- choose (1, 3 :: Int)
      foldM (const . edit) src [1 .. n]
    edit src = do
      i <- choose (0, BS.length src)
      j <- choose (i, min (BS.length src) (i + 3))
      p <- elements pieces
      pure (BS.take i src <> p <> BS.drop j src)
    pieces = ["", "\n", "\n  ", " ", "  ", "x", "Y", "1", "_", "\t", "\r", "\195\169", "--"] <> keywords <> BC.words symbols
