{-# LANGUAGE OverloadedStrings #-}

module ParserTests (tests) where

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (toList)
import Racc.Diagnostic (Diagnostic (Diagnostic), Located (Located), Pos (Pos), Span (Span))
import Racc.Lexer (tokenize)
import Racc.Parser (parse)
import Racc.Syntax
  ( BinaryOp (Add, And, BitAnd, BitOr, BitXor, Div, Mod, Mul, Or, Pow, ShiftLeft, ShiftRight, Sub),
    Block (Block),
    Body (BlockBody, ExprBody),
    CompareOp (Equal, Greater, GreaterEqual, Less, LessEqual, NotEqual),
    Constructor (Constructor),
    Decl (EquationDecl, SignatureDecl, TypeAlias, TypeDecl),
    Equation (Equation),
    Expr (App, Binary, Compare, Con, IfExpr, Lit, Range, Tuple, Unary, Unit, Var),
    Guard (Guard, Otherwise),
    Pattern (PAs, PCon, PLit, POr, PTuple, PUnit, PVar, PWild),
    RangeOp (Downto, To, Until),
    Rhs (Guarded, Plain),
    Signature (Signature),
    StateLine (StateSignature, StateVar),
    Stmt (Bind, Break, Continue, ExprStmt, For, If, LocalEquation, LocalSignature, Match, Pass, Return, While),
    Type (TyCon, TyFun, TyTuple, TyUnit, TyVar),
    UnaryOp (Complement, Neg, Not),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "parser" $
    [testCase (show src) (parsed ("x = " <> src <> "\n") @?= Right ("(eq x () " <> e <> ")")) | (src, e) <- expressions]
      <> [testCase (show src) (parsed ("f =\n" <> indent src) @?= Right ("(eq f () (block " <> e <> "))")) | (src, e) <- statements]
      <> [testCase (show src) (parsed src @?= Right e) | (src, e) <- declarations]
      <> [testCase (show src) (failure src @?= e) | (src, e) <- rejected]
      <> [testCase "spans" spans]
  where
    indent = BC.unlines . map ("  " <>) . BC.lines

expressions :: [(ByteString, String)]
expressions =
  [ ("a or b or c and not d == e", "(or (or a b) (and c (not (cmp d == e))))"),
    ("a == b != c < d <= e > f >= g + 1", "(cmp a == b != c < d <= e > f >= (+ g 1))"),
    ("(1 to n by 2, a until b, a downto b)", "(tuple (to 1 n 2) (until a b) (downto a b))"),
    ("a | b ^ c & d << e + f * g", "(| a (^ b (& c (<< d (+ e (* f g))))))"),
    ("a >> b - c - d / e % f", "(>> a (- (- b c) (% (/ d e) f)))"),
    ("-a ** b ** ~c", "(neg (** a (** b (~ c))))"),
    ("f A -b ~c", "(f A (neg b) (~ c))"),
    ("f (g a) ** 2", "(** (f (g a)) 2)"),
    ("((a), (), B 1)", "(tuple a () (B 1))"),
    ("if a then b else c + 1", "(if a b (+ c 1))")
  ]

-- | Lines of a block.
statements :: [(ByteString, String)]
statements =
  [ ("g : Int -> Int\ng y = y\ng 1", "(sig g (-> Int Int)) (eq g (y) y) (g 1)"),
    ("g y\n  | y > 0 = 1\n  | otherwise = 2\ng", "(eq g (y) (guards ((cmp y > 0) 1) (otherwise 2))) g"),
    ("g =\n  state\n    s = 1\n  s\ng", "(eq g () (block (state (var s 1)) s)) g"),
    ("(a, _) = p\nJust (P b) = q\nA or B as c = r\nx as y = t\nx or y = u\n(-1, ()) = s\na", "(bind (tuple a _) p) (bind (Just (P b)) q) (bind (as (or A B) c) r) (bind (as x y) t) (bind (or x y) u) (bind (tuple -1 ()) s) a"),
    ("if (if a then b else c)\n  d\nif e\n  f\nelif g\n  h\nelse\n  i", "(if ((if a b c) (block d))) (if (e (block f)) (g (block h)) (else (block i)))"),
    ("if a then b else c\nFoo 1", "(if a b c) (Foo 1)"),
    ("while a\n  b\nfor (i, j) in 1 to n\n  pass", "(while a (block b)) (for (tuple i j) (to 1 n) (block pass))"),
    ("match x\n  Just y -> y\n  N\n    | a -> 1\n    | otherwise -> 2\n  _ ->\n    pass", "(match x ((Just y) y) (N (guards (a 1) (otherwise 2))) (_ (block pass)))"),
    ("return\nreturn a\nbreak\nbreak 2\ncontinue\ncontinue 2", "(return) (return a) (break) (break 2) (continue) (continue 2)")
  ]

declarations :: [(ByteString, String)]
declarations =
  [ ("", ""),
    ("f : Maybe (List a) -> (Int, b) -> ()\n", "(sig f (-> (Maybe (List a)) (-> (tuple Int b) ())))"),
    ("f 0 = 1\nf -1 = 2\nf (x, y) N () _ = x\n", "(eq f (0) 1) (eq f (-1) 2) (eq f ((tuple x y) N () _) x)"),
    ("f x\n  | x =\n    state\n      s : Int\n      s = 1\n    s\n  | otherwise = 2\n", "(eq f (x) (guards (x (block (state (sig s Int) (var s 1)) s)) (otherwise 2)))"),
    ("type Maybe a\n  Nothing\n  Just a\ntype T\n  C (List Int) a\n", "(type Maybe (a) (Nothing) (Just a)) (type T () (C (List Int) a))"),
    ("type Pair a b = (a, b)\n", "(alias Pair (a b) (tuple a b))")
  ]

-- | One input for each way the parser can fail, with its error.
rejected :: [(ByteString, String)]
rejected =
  [ ("  f = 1\n", "1:3: expected a declaration, found a more indented line"),
    ("type x\n", "1:6: expected a type name after `type`, found `x`"),
    ("type T x 1\n", "1:10: expected a type parameter, `=`, or the end of the line after `x`, found `1`"),
    ("type T\nx\n", "2:1: expected indented constructors, found `x`"),
    ("type T\n  c\n", "2:3: expected a constructor, found `c`"),
    ("f : Int\n  requires x\n", "2:3: signature clauses are not part of version 0.1"),
    ("f : Eq a => a\n", "1:10: type class constraints are not part of version 0.1"),
    ("f : -> Int\n", "1:5: expected a type after `:`, found `->`"),
    ("f\n  x\n", "1:2: expected `=` or guards after `f`, found the end of the line"),
    ("f =\n", "1:4: expected an indented block, found the end of the file"),
    ("f\n  | a = 1\n  b = 2\n", "3:3: expected `|`, found `b`"),
    ("f\n  | a 1\n", "2:8: expected `=` after `1`, found the end of the line"),
    ("f =\n  if a\n    state\n      x = 1\n    x\n", "3:5: `state` is only allowed at the start of a function body"),
    ("f =\n  state\n  x\n", "3:3: expected indented state variables, found `x`"),
    ("f =\n  state\n    1\n  x\n", "3:5: expected a name, found `1`"),
    ("f =\n  state\n    x 1\n  x\n", "3:7: expected `=` after `x`, found `1`"),
    ("f =\n  yield x\n", "2:3: generators are not part of version 0.1"),
    ("f =\n  for x y\n    b\n", "2:9: expected `in` after `x`, found `y`"),
    ("f =\n  [a] = p\n", "2:3: list patterns are not part of version 0.1"),
    ("f =\n  - a = p\n", "2:5: expected a number after `-`, found `a`"),
    ("f =\n  while a\n    invariant b\n    c\n", "3:5: loop invariants are not part of version 0.1"),
    ("f =\n  break x\n", "2:9: expected a number or the end of the line after `break`, found `x`"),
    ("x = (+ 1)\n", "1:6: operator sections are not part of version 0.1"),
    ("x = (a +)\n", "1:9: operator sections are not part of version 0.1"),
    ("x = (a -)\n", "1:9: operator sections are not part of version 0.1"),
    ("x = (a | b <- c)\n", "1:12: comprehensions are not part of version 0.1"),
    ("x = [1]\n", "1:5: lists are not part of version 0.1"),
    ("x = {}\n", "1:5: sets and maps are not part of version 0.1"),
    ("x = xs[1]\n", "1:7: indexing is not part of version 0.1"),
    ("x = \\a -> a\n", "1:5: lambdas are not part of version 0.1"),
    ("x = from 1\n", "1:5: ranges without an end are not part of version 0.1"),
    ("x = f result\n", "1:7: `result` is not part of version 0.1"),
    ("x = if a b else c\n", "1:12: expected `then` after `b`, found `else`"),
    ("x = if a then b c\n", "1:18: expected `else` after `c`, found the end of the line"),
    ("x = 1 +\n", "1:8: expected an expression after `+`, found the end of the line"),
    ("x = (1, )\n", "1:9: expected an expression after `,`, found `)`"),
    ("x = (1, 2\n", "1:10: expected `,` or `)` after `2`, found the end of the line"),
    ("f =\n  match x\n  y\n", "3:3: expected indented match arms, found `y`"),
    ("f =\n  (a, =) = p\n", "2:7: expected a pattern after `,`, found `=`"),
    ("f =\n  (a, b) c = d\n", "2:10: expected `=` after `)`, found `c`"),
    ("f : T[a]\n", "1:6: expected the end of the line after `T`, found `[`"),
    ("f =\n  match x\n    A ->\n  y\n", "4:3: expected an indented block, found a less indented line")
  ]

-- | Spans cover the node's tokens, without the line break or the next line.
spans :: IO ()
spans = do
  let decls = either (error . show) id (parse =<< tokenize "f =\n  if a\n    b\n  x + y\n")
  case decls of
    [Located d (EquationDecl (Equation _ _ (Plain (BlockBody (Block _ ss)))))] -> do
      d @?= Span (Pos 1 1) (Pos 4 8)
      map (\(Located sp _) -> sp) (toList ss) @?= [Span (Pos 2 3) (Pos 3 6), Span (Pos 4 3) (Pos 4 8)]
    _ -> assertBool "unexpected tree" False

failure :: ByteString -> String
failure = either (\(Diagnostic (Span (Pos l c) _) m) -> show l <> ":" <> show c <> ": " <> m) (const "accepted") . (parse <=< tokenize)

parsed :: ByteString -> Either String String
parsed = either (Left . show) (Right . unwords . map decl) . (parse <=< tokenize)

-- * Printing the tree as S-expressions

node :: [String] -> String
node xs = "(" <> unwords xs <> ")"

name :: Located ByteString -> String
name (Located _ n) = BC.unpack n

decl :: Located Decl -> String
decl (Located _ d) = case d of
  SignatureDecl s -> signature s
  EquationDecl e -> equation e
  TypeDecl n vs cs -> node (["type", name n, node (map name vs)] <> [node (name c : map typ ts) | Constructor c ts <- toList cs])
  TypeAlias n vs t -> node ["alias", name n, node (map name vs), typ t]

signature :: Signature -> String
signature (Signature n t) = node ["sig", name n, typ t]

equation :: Equation -> String
equation (Equation n ps r) = node ["eq", name n, node (map pat ps), rhs r]

rhs :: Rhs -> String
rhs = \case
  Plain b -> body b
  Guarded gs -> node ("guards" : [node [guard g, body b] | (g, b) <- toList gs])
  where
    guard = \case
      Otherwise -> "otherwise"
      Guard e -> expr e

body :: Body -> String
body = \case
  ExprBody e -> expr e
  BlockBody b -> block b

block :: Block -> String
block (Block sv ss) = node ("block" : [node ("state" : map stateLine sv) | not (null sv)] <> map stmt (toList ss))
  where
    stateLine = \case
      StateSignature s -> signature s
      StateVar n b -> node ["var", name n, body b]

stmt :: Located Stmt -> String
stmt (Located _ s) = case s of
  LocalSignature sg -> signature sg
  LocalEquation e -> equation e
  Bind p b -> node ["bind", pat p, body b]
  If bs e -> node ("if" : [node [expr c, block b] | (c, b) <- toList bs] <> [node ["else", block b] | Just b <- [e]])
  While c b -> node ["while", expr c, block b]
  For p e b -> node ["for", pat p, expr e, block b]
  Match e arms -> node ("match" : expr e : [node [pat p, rhs r] | (p, r) <- toList arms])
  Return e -> node ("return" : map expr (toList e))
  Break n -> node ("break" : [show i | Located _ i <- toList n])
  Continue n -> node ("continue" : [show i | Located _ i <- toList n])
  Pass -> "pass"
  ExprStmt e -> expr e

expr :: Located Expr -> String
expr (Located _ e) = case e of
  Var n -> BC.unpack n
  Con n -> BC.unpack n
  Lit n -> show n
  Unit -> "()"
  Tuple es -> node ("tuple" : map expr es)
  App f as -> node (map expr (f : toList as))
  Unary op a -> node [unaryOp op, expr a]
  Binary op a b -> node [binaryOp op, expr a, expr b]
  Compare a rest -> node ("cmp" : expr a : concat [[compareOp op, expr b] | (op, b) <- toList rest])
  Range a op b c -> node ([rangeOp op, expr a, expr b] <> map expr (toList c))
  IfExpr c a b -> node ["if", expr c, expr a, expr b]
  where
    unaryOp :: UnaryOp -> String
    unaryOp = \case
      Neg -> "neg"
      Complement -> "~"
      Not -> "not"
    binaryOp :: BinaryOp -> String
    binaryOp = \case
      Or -> "or"
      And -> "and"
      BitOr -> "|"
      BitXor -> "^"
      BitAnd -> "&"
      ShiftLeft -> "<<"
      ShiftRight -> ">>"
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
      Div -> "/"
      Mod -> "%"
      Pow -> "**"
    compareOp :: CompareOp -> String
    compareOp = \case
      Equal -> "=="
      NotEqual -> "!="
      Less -> "<"
      LessEqual -> "<="
      Greater -> ">"
      GreaterEqual -> ">="
    rangeOp :: RangeOp -> String
    rangeOp = \case
      To -> "to"
      Until -> "until"
      Downto -> "downto"

pat :: Located Pattern -> String
pat (Located _ p) = case p of
  PWild -> "_"
  PVar n -> BC.unpack n
  PLit n -> show n
  PUnit -> "()"
  PCon n [] -> BC.unpack n
  PCon n ps -> node (BC.unpack n : map pat ps)
  PTuple ps -> node ("tuple" : map pat ps)
  POr ps -> node ("or" : map pat (toList ps))
  PAs q n -> node ["as", pat q, name n]

typ :: Located Type -> String
typ (Located _ t) = case t of
  TyVar n -> BC.unpack n
  TyCon n [] -> BC.unpack n
  TyCon n ts -> node (BC.unpack n : map typ ts)
  TyFun a b -> node ["->", typ a, typ b]
  TyUnit -> "()"
  TyTuple ts -> node ("tuple" : map typ ts)
