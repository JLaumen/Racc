{-# LANGUAGE OverloadedStrings #-}

-- | Turns source text into the tokens of GRAMMAR.md, including layout.
module Racc.Lexer (Token (..), tokenize) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Internal (w2c)
import Data.ByteString.Unsafe qualified as BU
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Racc.Diagnostic (Diagnostic (Diagnostic), Located (Located), Pos (Pos, col, line), Span (Span))

data Token
  = TLower ByteString
  | TUpper ByteString
  | TInt Integer
  | TKeyword ByteString
  | TSymbol ByteString
  | TIndex
  | TNeg
  | TNewline
  | TIndent
  | TDedent
  | TEnd
  deriving stock (Eq, Show)

tokenize :: ByteString -> Either Diagnostic [Located Token]
tokenize src = layout =<< scan src

keywords :: [ByteString]
keywords =
  [ "and",
    "as",
    "break",
    "by",
    "continue",
    "downto",
    "effects",
    "elif",
    "else",
    "ensures",
    "for",
    "from",
    "if",
    "in",
    "invariant",
    "match",
    "modifies",
    "not",
    "or",
    "otherwise",
    "pass",
    "requires",
    "result",
    "return",
    "state",
    "then",
    "to",
    "type",
    "until",
    "while",
    "yield"
  ]

symbols2 :: [ByteString]
symbols2 = ["->", "=>", "==", "!=", "<=", ">=", "<<", ">>", "**", "..", "<-"]

symbols1 :: [Char]
symbols1 = "=:|,()[]{}\\+-*/%<>&^~"

isNameChar :: Char -> Bool
isNameChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '\''

-- | Tokens without layout, with `[` and `-` not yet classified.
scan :: ByteString -> Either Diagnostic [Located Token]
scan src = go [] 0 1 0
  where
    n = BS.length src
    at i = if i < n then w2c (BU.unsafeIndex src i) else '\n'
    run p i = if i < n && p (at i) then run p (i + 1) else i
    slice i j = BS.take (j - i) (BS.drop i src)

    -- acc: tokens in reverse; i: offset; ln: line; ls: offset of the line start
    go acc i ln ls
      | i >= n = Right (reverse acc)
      | otherwise = case at i of
          ' ' -> go acc (i + 1) ln ls
          '\n' -> go acc (i + 1) (ln + 1) (i + 1)
          '\r'
            | i + 1 < n, at (i + 1) == '\n' -> go acc (i + 2) (ln + 1) (i + 2)
            | otherwise -> err i (i + 1) "carriage return without a line feed"
          '\t' -> err i (i + 1) "tab character; indent with spaces"
          '-' | at (i + 1) == '-' -> go acc (run (/= '\n') i) ln ls
          c
            | isDigit c -> number (run isNameChar i)
            | isAsciiLower c || c == '_' -> name TLower (run isNameChar i)
            | isAsciiUpper c -> name TUpper (run isNameChar i)
            | slice i (i + 2) `elem` symbols2 -> emit (i + 2) (TSymbol (slice i (i + 2)))
            | c `elem` symbols1 -> emit (i + 1) (TSymbol (slice i (i + 1)))
            | ord c >= 128 -> err i (i + 1) "non-ASCII character outside a comment"
            | otherwise -> err i (i + 1) ("unexpected character `" <> [c] <> "`")
      where
        pos j = Pos ln (j - ls + 1)
        err j k msg = Left (Diagnostic (Span (pos j) (pos k)) msg)
        emit j t = go (Located (Span (pos i) (pos j)) t : acc) j ln ls
        name con j
          | text == "_" = emit j (TSymbol text)
          | text `elem` keywords = emit j (TKeyword text)
          | otherwise = emit j (con text)
          where
            text = slice i j
        number j
          | BC.all (\c -> isDigit c || c == '_') text,
            not ("__" `BS.isInfixOf` text),
            BC.last text /= '_' =
              emit j (TInt (BC.foldl' digit 0 text))
          | otherwise = err i j ("invalid number literal `" <> BC.unpack text <> "`")
          where
            text = slice i j
            digit v '_' = v
            digit v c = v * 10 + toInteger (ord c - ord '0')

-- | Inserts NEWLINE, INDENT, DEDENT, and END, and classifies `[` and `-`.
layout :: [Located Token] -> Either Diagnostic [Located Token]
layout = go [] (1 :| []) 0 Nothing
  where
    go ::
      [Located Token] ->
      NonEmpty Int ->
      Int ->
      Maybe (Located Token) ->
      [Located Token] ->
      Either Diagnostic [Located Token]
    -- acc: output in reverse; stack: block columns; depth: open brackets;
    -- prev: the previous input token
    go acc stack _ prev [] = Right (reverse acc <> closing)
      where
        p = maybe (Pos 1 1) (\(Located (Span _ e) _) -> e) prev
        mark = Located (Span p p)
        closing =
          [mark TNewline | Just _ <- [prev]]
            <> (mark TDedent <$ NE.tail stack)
            <> [mark TEnd]
    go acc stack depth prev (Located sp@(Span s e) raw : rest)
      | depth == 0,
        newLine = case compare (col s) (NE.head stack) of
          GT -> continue (brk <> [here TIndent]) (NE.cons (col s) stack)
          EQ -> continue brk stack
          LT -> case NE.span (> col s) stack of
            (popped, top : more)
              | top == col s -> continue (brk <> (here TDedent <$ popped)) (top :| more)
            _ -> Left (Diagnostic sp "indentation does not match any enclosing block")
      | otherwise = continue [] stack
      where
        newLine = maybe True (\(Located (Span _ pe) _) -> line pe < line s) prev
        here = Located (Span s s)
        brk = [Located (Span pe pe) TNewline | Just (Located (Span _ pe) _) <- [prev]]
        spaceBefore = maybe True (\(Located (Span _ pe) _) -> pe /= s) prev
        spaceAfter = case rest of
          Located (Span ns _) _ : _ -> ns /= e
          [] -> True
        continue inserted stack' = go (Located sp tok : out) stack' depth' (Just (Located sp raw)) rest
          where
            out = reverse inserted <> acc
            tok = case raw of
              TSymbol "["
                | Just (Located (Span _ pe) pt) <- prev, pe == s, endsName pt -> TIndex
              TSymbol "-"
                | not (endsOperand out) || (spaceBefore && not spaceAfter) -> TNeg
              _ -> raw
            depth' = case tok of
              TIndex -> depth + 1
              TSymbol b
                | b `elem` ["(", "[", "{"] -> depth + 1
                | b `elem` [")", "]", "}"] -> max 0 (depth - 1)
              _ -> depth

    endsName = \case
      TLower _ -> True
      TUpper _ -> True
      TSymbol b -> b `elem` [")", "]"]
      _ -> False
    endsOperand = \case
      Located _ t : _ -> case t of
        TInt _ -> True
        TKeyword "result" -> True
        TSymbol "}" -> True
        _ -> endsName t
      [] -> False
