{-# LANGUAGE OverloadedStrings #-}

-- | Recursive descent over the tokens of "Racc.Lexer", following GRAMMAR.md
-- without backtracking. Parsing stops at the first error.
module Racc.Parser (parse) where

import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, put)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe)
import Racc.Diagnostic (Diagnostic (Diagnostic), Located (Located), Pos (Pos), Severity (Error), Span (Span), quote)
import Racc.Lexer (Token (TDedent, TEnd, TIndent, TIndex, TInt, TKeyword, TLower, TNeg, TNewline, TSymbol, TUpper))
import Racc.Syntax
  ( BinaryOp (Add, And, BitAnd, BitOr, BitXor, Div, Mod, Mul, Or, Pow, ShiftLeft, ShiftRight, Sub),
    Block (Block),
    Body (BlockBody, ExprBody),
    CompareOp (Equal, Greater, GreaterEqual, Less, LessEqual, NotEqual),
    Constructor (Constructor),
    Decl (EquationDecl, SignatureDecl, TypeAlias, TypeDecl),
    Equation (Equation),
    Expr (App, Binary, Compare, Con, IfExpr, Lit, Tuple, Unary, Unit, Var),
    Guard (Guard, Otherwise),
    Name,
    Pattern (PAs, PCon, PLit, POr, PTuple, PUnit, PVar, PWild),
    Range (Range),
    RangeOp (Downto, To, Until),
    Rhs (Guarded, Plain),
    Signature (Signature),
    StateLine (StateSignature, StateVar),
    Stmt (Bind, Break, Continue, ExprStmt, For, If, LocalEquation, LocalSignature, Match, Pass, Return, While),
    Type (TyCon, TyFun, TyTuple, TyVar),
    UnaryOp (Complement, Neg, Not),
  )

-- | The end of the last token consumed, that token unless it was layout, and
-- the remaining tokens, which always end with END.
data S = S !Pos !(Maybe Token) [Located Token]

type P = StateT S (Either Diagnostic)

parse :: [Located Token] -> Either Diagnostic [Located (Decl Name)]
parse ts = evalStateT (manyWhile (/= TEnd) (located decl)) (S (Pos 1 1) Nothing ts)

-- * Declarations

decl :: P (Decl Name)
decl =
  peek >>= \case
    TKeyword "type" -> next >> typeDecl
    TLower _ -> do
      t <- peekAt 1
      if t == TSymbol ":" then SignatureDecl <$> signature else EquationDecl <$> equation
    _ -> expected "a declaration"

signature :: P (Signature Name)
signature = do
  n <- name
  next
  t <- typ
  peek >>= \case
    TSymbol "=>" -> notIn01 "type class constraints are"
    _ -> endLine
  peek >>= \case
    TIndent -> next >> notIn01 "signature clauses are"
    _ -> pure (Signature n t)

equation :: P (Equation Name)
equation = do
  n <- name
  ps <- manyWhile startsApat apat
  Equation n ps <$> rhs "=" True

typeDecl :: P (Decl Name)
typeDecl = do
  n <- upper "a type name"
  vs <- manyWhile isLower name
  peek >>= \case
    TSymbol "=" -> next >> TypeAlias n vs <$> typ <* endLine
    TNewline -> next >> TypeDecl n vs <$> indented "indented constructors" constructor
    _ -> expected "a type parameter, `=`, or the end of the line"
  where
    constructor = Constructor <$> upper "a constructor" <*> manyWhile startsAtype atype <* endLine

-- | The rest of an equation (`sep` is `=`) or of a match arm (`sep` is `->`).
-- `st` tells whether a block may start with `state`.
rhs :: ByteString -> Bool -> P (Rhs Name)
rhs sep st = do
  ts <- traverse peekAt [0, 1, 2]
  case ts of
    TSymbol x : _ | x == sep -> next >> Plain <$> body st
    [TNewline, TIndent, TSymbol "|"] -> next >> next >> Guarded <$> untilDedent guardLine
    _ -> expected (quote sep <> " or guards")
  where
    guardLine = do
      expect (TSymbol "|") "`|`"
      g <-
        peek >>= \case
          TKeyword "otherwise" -> Otherwise <$ next
          _ -> Guard <$> expr
      expect (TSymbol sep) (quote sep)
      (g,) <$> body st

body :: Bool -> P (Body Name)
body st =
  peek >>= \case
    TNewline -> next >> BlockBody <$> block st
    _ -> ExprBody <$> expr <* endLine

block :: Bool -> P (Block Name)
block st = do
  expect TIndent "an indented block"
  sv <-
    peek >>= \case
      TKeyword "state" | st -> next >> endLine >> NE.toList <$> indented "indented state variables" stateLine
      _ -> pure []
  Block sv <$> untilDedent (located stmt)
  where
    stateLine = do
      t <- peekAt 1
      if t == TSymbol ":"
        then StateSignature <$> signature
        else StateVar <$> name <* expect (TSymbol "=") "`=`" <*> body False

-- * Statements

stmt :: P (Stmt Name)
stmt =
  peek >>= \case
    TKeyword "if" -> do
      s <- start
      next
      c <- expr
      peek >>= \case
        TKeyword "then" -> ExprStmt <$> inlineIf s c <* endLine
        _ -> ifStmt c
    TKeyword "while" -> next >> While <$> expr <* endLine <*> block False
    TKeyword "for" -> next >> For <$> pat <* expect (TKeyword "in") "`in`" <*> range <* endLine <*> block False
    TKeyword "match" ->
      next >> Match <$> expr <* endLine <*> indented "indented match arms" ((,) <$> pat <*> rhs "->" False)
    TKeyword "return" -> next >> Return <$> unlessEnd expr <* endLine
    TKeyword "break" -> next >> Break <$> unlessEnd int <* endLine
    TKeyword "continue" -> next >> Continue <$> unlessEnd int <* endLine
    TKeyword "pass" -> next >> Pass <$ endLine
    TKeyword "state" -> failHere "`state` is only allowed at the start of a function body"
    TKeyword "yield" -> notIn01 "generators are"
    TKeyword "invariant" -> notIn01 "loop invariants are"
    TLower _ -> do
      t <- peekAt 1
      d <- defines
      if t == TSymbol ":"
        then LocalSignature <$> signature
        else
          if d && t `notElem` map TKeyword ["as", "or"]
            then LocalEquation <$> equation
            else bindOrExpr d
    _ -> defines >>= bindOrExpr
  where
    bindOrExpr d =
      if d
        then Bind <$> pat <* expect (TSymbol "=") "`=`" <*> body False
        else ExprStmt <$> expr <* endLine
    unlessEnd :: P a -> P (Maybe a)
    unlessEnd p =
      peek >>= \case
        TNewline -> pure Nothing
        _ -> Just <$> p
    int = satisfy "a number or the end of the line" (\case TInt n -> Just n; _ -> Nothing)

-- | The rest of an `if` statement, after its first condition.
ifStmt :: Located (Expr Name) -> P (Stmt Name)
ifStmt c = do
  b <- branch c
  bs <- manyWhile (== TKeyword "elif") (next >> expr >>= branch)
  e <-
    peek >>= \case
      TKeyword "else" -> next >> endLine >> Just <$> block False
      _ -> pure Nothing
  pure (If (b :| bs) e)
  where
    branch :: Located (Expr Name) -> P (Located (Expr Name), Block Name)
    branch cond = (cond,) <$> (endLine >> block False)

-- | Whether the statement ahead defines something: its line contains `=`,
-- or guards follow it.
defines :: P Bool
defines =
  gets $ \(S _ _ ts) ->
    let (line, rest) = break ((== TNewline) . token) ts
     in any ((== TSymbol "=") . token) line || map token (take 2 (drop 1 rest)) == [TIndent, TSymbol "|"]

-- * Expressions

expr :: P (Located (Expr Name))
expr =
  peek >>= \case
    TKeyword "if" -> do
      s <- start
      next
      expr >>= inlineIf s
    TSymbol "\\" -> notIn01 "lambdas are"
    _ -> foldr binary notExpr [[(TKeyword "or", Or)], [(TKeyword "and", And)]]

-- | The rest of `if c then a else b` that starts at `s`, after `c`.
inlineIf :: Pos -> Located (Expr Name) -> P (Located (Expr Name))
inlineIf s c = do
  expect (TKeyword "then") "`then`"
  a <- expr
  expect (TKeyword "else") "`else`"
  b@(Located (Span _ e) _) <- expr
  pure (Located (Span s e) (IfExpr c a b))

notExpr :: P (Located (Expr Name))
notExpr =
  peek >>= \case
    TKeyword "not" -> prefix Not notExpr
    _ -> do
      a <- rangeExpr
      rest <- chain
      pure $ case nonEmpty rest of
        Nothing -> a
        Just r -> Located (cover a (snd (NE.last r))) (Compare a r)
  where
    chain =
      peek >>= \t -> case lookup t compareOps of
        Just op -> next >> rangeExpr >>= \b -> ((op, b) :) <$> chain
        Nothing -> pure []

rangeExpr :: P (Located (Expr Name))
rangeExpr = do
  a <- arith
  t <- peek
  if isJust (lookup t rangeOps) then notIn01 "ranges anywhere but directly after `for ... in` are" else pure a

-- | The range after `for ... in`, the only place for one in version 0.1.
range :: P (Range Name)
range = do
  a@(Located sa _) <- arith
  t <- peek
  op <- maybe (notIn01At sa "loops over anything but a range are") pure (lookup t rangeOps)
  next
  b <- arith
  c <-
    peek >>= \case
      TKeyword "by" -> next >> Just <$> arith
      _ -> pure Nothing
  pure (Range a op b c)

arith :: P (Located (Expr Name))
arith = foldr binary unary arithOps

-- | One level of left-associative binary operators.
binary :: [(Token, BinaryOp)] -> P (Located (Expr Name)) -> P (Located (Expr Name))
binary ops sub = sub >>= go
  where
    go l =
      peek >>= \t -> case lookup t ops of
        Just op -> next >> sub >>= \r -> go (Located (cover l r) (Binary op l r))
        Nothing -> pure l

unary :: P (Located (Expr Name))
unary =
  peek >>= \case
    TNeg -> prefix Neg unary
    TSymbol "~" -> prefix Complement unary
    _ -> do
      a <- app
      peek >>= \case
        TSymbol "**" -> next >> unary >>= \b -> pure (Located (cover a b) (Binary Pow a b))
        _ -> pure a

app :: P (Located (Expr Name))
app = do
  f <- postfix
  args <- manyWhile startsArg arg
  pure $ case nonEmpty args of
    Nothing -> f
    Just as -> Located (cover f (NE.last as)) (App f as)
  where
    startsArg t = startsAtom t || t `elem` [TNeg, TSymbol "~"]
    arg =
      peek >>= \case
        TNeg -> prefix Neg postfix
        TSymbol "~" -> prefix Complement postfix
        _ -> postfix

postfix :: P (Located (Expr Name))
postfix = do
  a <- atom
  peek >>= \case
    TIndex -> notIn01 "indexing is"
    _ -> pure a

atom :: P (Located (Expr Name))
atom =
  peek >>= \case
    TInt n -> leaf (Lit n)
    TLower n -> leaf (Var n)
    TUpper n -> leaf (Con n)
    TSymbol "(" -> do
      t <- peekAt 1
      if isOperator t
        then next >> notIn01 "operator sections are"
        else parenthesized (pure Unit) Tuple expr
    TSymbol "[" -> notIn01 "lists are"
    TSymbol "{" -> notIn01 "sets and maps are"
    TKeyword "from" -> notIn01 "ranges without an end are"
    TKeyword "result" -> notIn01 "`result` is"
    TSymbol ")" -> do
      S _ prev _ <- get
      if maybe False isOperator prev then notIn01 "operator sections are" else expected "an expression"
    _ -> expected "an expression"

-- | Whether a token is an operator that can appear in an operator section.
isOperator :: Token -> Bool
isOperator t = t `elem` (TSymbol "**" : TNeg : map fst (concat arithOps) <> map fst compareOps)

startsAtom :: Token -> Bool
startsAtom = \case
  TInt _ -> True
  TLower _ -> True
  TUpper _ -> True
  TSymbol b -> b `elem` ["(", "[", "{"]
  TKeyword k -> k `elem` ["from", "result"]
  _ -> False

prefix :: UnaryOp -> P (Located (Expr Name)) -> P (Located (Expr Name))
prefix op p = do
  s <- start
  next
  e@(Located (Span _ end) _) <- p
  pure (Located (Span s end) (Unary op e))

arithOps :: [[(Token, BinaryOp)]]
arithOps =
  map (map (first TSymbol)) $
    [[("|", BitOr)], [("^", BitXor)], [("&", BitAnd)], [("<<", ShiftLeft), (">>", ShiftRight)]]
      <> [[("+", Add), ("-", Sub)], [("*", Mul), ("/", Div), ("%", Mod)]]

compareOps :: [(Token, CompareOp)]
compareOps =
  map (first TSymbol) [("==", Equal), ("!=", NotEqual), ("<", Less), ("<=", LessEqual), (">", Greater), (">=", GreaterEqual)]

rangeOps :: [(Token, RangeOp)]
rangeOps = map (first TKeyword) [("to", To), ("until", Until), ("downto", Downto)]

-- * Patterns

pat :: P (Located (Pattern Name))
pat = do
  p <- conPattern
  ps <- manyWhile (== TKeyword "or") (next >> conPattern)
  let q = case nonEmpty ps of
        Nothing -> p
        Just r -> Located (cover p (NE.last r)) (POr (p :| ps))
  peek >>= \case
    TKeyword "as" -> next >> name >>= \n -> pure (Located (cover q n) (PAs q n))
    _ -> pure q
  where
    conPattern =
      peek >>= \case
        TUpper n -> located (next >> PCon n <$> manyWhile startsApat apat)
        _ -> apat

apat :: P (Located (Pattern Name))
apat =
  peek >>= \case
    TSymbol "_" -> leaf PWild
    TLower n -> leaf (PVar n)
    TInt n -> leaf (PLit n)
    TNeg -> located (next >> (\(Located _ n) -> PLit (negate n)) <$> satisfy "a number" (\case TInt n -> Just n; _ -> Nothing))
    TUpper n -> leaf (PCon n [])
    TSymbol "(" -> parenthesized (pure PUnit) PTuple pat
    TSymbol "[" -> notIn01 "list patterns are"
    _ -> expected "a pattern"

startsApat :: Token -> Bool
startsApat = \case
  TLower _ -> True
  TUpper _ -> True
  TInt _ -> True
  TNeg -> True
  TSymbol b -> b `elem` ["_", "(", "["]
  _ -> False

-- * Types

typ :: P (Located Type)
typ = do
  a <-
    peek >>= \case
      TUpper n -> located (next >> TyCon n <$> manyWhile startsAtype atype)
      _ -> atype
  peek >>= \case
    TSymbol "->" -> next >> typ >>= \b -> pure (Located (cover a b) (TyFun a b))
    _ -> pure a

atype :: P (Located Type)
atype =
  peek >>= \case
    TLower n -> leaf (TyVar n)
    TUpper n -> leaf (TyCon n [])
    TSymbol "(" -> parenthesized (expected "a type") TyTuple typ
    _ -> expected "a type"

startsAtype :: Token -> Bool
startsAtype = \case
  TLower _ -> True
  TUpper _ -> True
  t -> t == TSymbol "("

-- * Building blocks

current :: P (Located Token)
current =
  gets $ \(S e _ ts) -> case ts of
    t : _ -> t
    [] -> Located (Span e e) TEnd

peek :: P Token
peek = token <$> current

peekAt :: Int -> P Token
peekAt n = gets (\(S _ _ ts) -> maybe TEnd token (listToMaybe (drop n ts)))

-- | Consumes the current token, except END.
next :: P ()
next = do
  S e prev ts <- get
  case ts of
    Located (Span _ e') t : rest@(_ : _)
      | t `elem` [TNewline, TIndent, TDedent] -> put (S e Nothing rest)
      | otherwise -> put (S e' (Just t) rest)
    _ -> put (S e prev ts)

token :: Located Token -> Token
token (Located _ t) = t

-- | The start of the current token.
start :: P Pos
start = (\(Located (Span s _) _) -> s) <$> current

located :: P a -> P (Located a)
located p = do
  s <- start
  x <- p
  S e _ _ <- get
  pure (Located (Span s e) x)

leaf :: a -> P (Located a)
leaf x = located (x <$ next)

cover :: Located a -> Located b -> Span
cover (Located (Span s _) _) (Located (Span _ e) _) = Span s e

satisfy :: String -> (Token -> Maybe a) -> P (Located a)
satisfy what f = do
  Located sp t <- current
  maybe (expected what) (\x -> Located sp x <$ next) (f t)

expect :: Token -> String -> P ()
expect t what = void $ satisfy what (\u -> if u == t then Just () else Nothing)

name :: P (Located Name)
name = satisfy "a name" (\case TLower n -> Just n; _ -> Nothing)

upper :: String -> P (Located Name)
upper what = satisfy what (\case TUpper n -> Just n; _ -> Nothing)

isLower :: Token -> Bool
isLower = \case
  TLower _ -> True
  _ -> False

endLine :: P ()
endLine = expect TNewline "the end of the line"

manyWhile :: (Token -> Bool) -> P a -> P [a]
manyWhile ok p = do
  t <- peek
  if ok t then (:) <$> p <*> manyWhile ok p else pure []

-- | One or more items up to and including DEDENT.
untilDedent :: P a -> P (NonEmpty a)
untilDedent p = do
  x <- p
  xs <- manyWhile (/= TDedent) p
  (x :| xs) <$ next

indented :: String -> P a -> P (NonEmpty a)
indented what p = expect TIndent what >> untilDedent p

-- | `()`, `(x)`, or a tuple `(x, y, ...)`.
parenthesized :: P a -> ([Located a] -> a) -> P (Located a) -> P (Located a)
parenthesized unit tuple item = located $ do
  next
  peek >>= \case
    TSymbol ")" -> unit <* next
    _ -> do
      x@(Located _ v) <- item
      xs <- manyWhile (== TSymbol ",") (next >> item)
      t <- peek
      when (t == TSymbol "<-") (notIn01 "comprehensions are")
      expect (TSymbol ")") "`,` or `)`"
      pure (if null xs then v else tuple (x : xs))

-- * Errors

failHere :: String -> P a
failHere msg = do
  Located sp _ <- current
  lift (Left (Diagnostic Error sp msg))

expected :: String -> P a
expected what = do
  Located _ t <- current
  S _ prev _ <- get
  failHere ("expected " <> what <> maybe "" ((" after " <>) . describe) prev <> ", found " <> describe t)

notIn01 :: String -> P a
notIn01 what = current >>= \(Located s _) -> notIn01At s what

notIn01At :: Span -> String -> P a
notIn01At s what = lift (Left (Diagnostic Error s (what <> " not part of version 0.1")))

describe :: Token -> String
describe = \case
  TLower n -> quote n
  TUpper n -> quote n
  TKeyword n -> quote n
  TSymbol n -> quote n
  TInt n -> quote (BC.pack (show n))
  TIndex -> "`[`"
  TNeg -> "`-`"
  TNewline -> "the end of the line"
  TIndent -> "a more indented line"
  TDedent -> "a less indented line"
  TEnd -> "the end of the file"
