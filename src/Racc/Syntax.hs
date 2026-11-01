{-# LANGUAGE RoleAnnotations #-}

-- | The syntax tree of version 0.1, mirroring GRAMMAR.md. Variables are
-- names after parsing, and references after name resolution.
module Racc.Syntax
  ( Name,
    Variable (..),
    Decl (..),
    Signature (..),
    Equation (..),
    Constructor (..),
    Rhs (..),
    Guard (..),
    Body (..),
    Block (..),
    StateLine (..),
    Stmt (..),
    Expr (..),
    UnaryOp (..),
    BinaryOp (..),
    CompareOp (..),
    RangeOp (..),
    Pattern (..),
    Type (..),
    sexpr,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Racc.Diagnostic (Located (Located))

type Name = ByteString

type role Decl representational

data Decl v
  = SignatureDecl (Signature v)
  | EquationDecl (Equation v)
  | TypeDecl (Located Name) [Located Name] (NonEmpty Constructor)
  | TypeAlias (Located Name) [Located Name] (Located Type)

type role Signature representational

data Signature v = Signature (Located v) (Located Type)

-- | One equation; consecutive ones are grouped into a function later.
type role Equation representational

data Equation v = Equation (Located v) [Located (Pattern v)] (Rhs v)

data Constructor = Constructor (Located Name) [Located Type]

-- | What follows the parameters of an equation, or the pattern of an arm.
type role Rhs representational

data Rhs v = Plain (Body v) | Guarded (NonEmpty (Guard v, Body v))

type role Guard representational

data Guard v = Otherwise | Guard (Located (Expr v))

type role Body representational

data Body v = ExprBody (Located (Expr v)) | BlockBody (Block v)

type role Block representational

data Block v = Block [StateLine v] (NonEmpty (Located (Stmt v)))

type role StateLine representational

data StateLine v = StateSignature (Signature v) | StateVar (Located v) (Body v)

type role Stmt representational

data Stmt v
  = LocalSignature (Signature v)
  | -- | Also `x = e`, which may turn out to be an assignment.
    LocalEquation (Equation v)
  | -- | A binding, or an assignment if the pattern's variables are places.
    Bind (Located (Pattern v)) (Body v)
  | -- | The `if` and `elif` branches, and the `else` branch.
    If (NonEmpty (Located (Expr v), Block v)) (Maybe (Block v))
  | While (Located (Expr v)) (Block v)
  | For (Located (Pattern v)) (Located (Expr v)) (Block v)
  | Match (Located (Expr v)) (NonEmpty (Located (Pattern v), Rhs v))
  | Return (Maybe (Located (Expr v)))
  | Break (Maybe (Located Integer))
  | Continue (Maybe (Located Integer))
  | Pass
  | ExprStmt (Located (Expr v))

type role Expr representational

data Expr v
  = Var v
  | Con Name
  | Lit Integer
  | Unit
  | -- | Two or more elements.
    Tuple [Located (Expr v)]
  | App (Located (Expr v)) (NonEmpty (Located (Expr v)))
  | Unary UnaryOp (Located (Expr v))
  | Binary BinaryOp (Located (Expr v)) (Located (Expr v))
  | -- | A chain such as `a < b <= c`.
    Compare (Located (Expr v)) (NonEmpty (CompareOp, Located (Expr v)))
  | -- | The optional last part is the step (`by`).
    Range (Located (Expr v)) RangeOp (Located (Expr v)) (Maybe (Located (Expr v)))
  | IfExpr (Located (Expr v)) (Located (Expr v)) (Located (Expr v))

data UnaryOp = Neg | Complement | Not

data BinaryOp
  = Or
  | And
  | BitOr
  | BitXor
  | BitAnd
  | ShiftLeft
  | ShiftRight
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow

data CompareOp = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual

data RangeOp = To | Until | Downto

type role Pattern representational

data Pattern v
  = PWild
  | PVar v
  | PLit Integer
  | PUnit
  | PCon Name [Located (Pattern v)]
  | -- | Two or more elements.
    PTuple [Located (Pattern v)]
  | -- | Two or more alternatives.
    POr (NonEmpty (Located (Pattern v)))
  | PAs (Located (Pattern v)) (Located v)

data Type
  = TyVar Name
  | TyCon Name [Located Type]
  | TyFun (Located Type) (Located Type)
  | -- | Two or more elements.
    TyTuple [Located Type]

-- * Printing as S-expressions

-- | How a variable is printed.
class Variable v where
  variable :: v -> String

instance Variable ByteString where
  variable = BC.unpack

-- | A declaration as one line, such as @(eq f (x) (+ x 1))@.
sexpr :: (Variable v) => Located (Decl v) -> String
sexpr (Located _ d) = case d of
  SignatureDecl s -> signature s
  EquationDecl e -> equation e
  TypeDecl n vs cs -> node (["type", var n, node (map var vs)] <> [node (var c : map typ ts) | Constructor c ts <- toList cs])
  TypeAlias n vs t -> node ["alias", var n, node (map var vs), typ t]

node :: [String] -> String
node xs = "(" <> unwords xs <> ")"

var :: (Variable v) => Located v -> String
var (Located _ x) = variable x

signature :: (Variable v) => Signature v -> String
signature (Signature n t) = node ["sig", var n, typ t]

equation :: (Variable v) => Equation v -> String
equation (Equation n ps r) = node ["eq", var n, node (map pat ps), rhs r]

rhs :: (Variable v) => Rhs v -> String
rhs = \case
  Plain b -> body b
  Guarded gs -> node ("guards" : [node [guard g, body b] | (g, b) <- toList gs])
  where
    guard :: (Variable w) => Guard w -> String
    guard = \case
      Otherwise -> "otherwise"
      Guard e -> expr e

body :: (Variable v) => Body v -> String
body = \case
  ExprBody e -> expr e
  BlockBody b -> block b

block :: (Variable v) => Block v -> String
block (Block sv ss) = node ("block" : [node ("state" : map stateLine sv) | not (null sv)] <> map stmt (toList ss))
  where
    stateLine :: (Variable w) => StateLine w -> String
    stateLine = \case
      StateSignature s -> signature s
      StateVar n b -> node ["var", var n, body b]

stmt :: (Variable v) => Located (Stmt v) -> String
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

expr :: (Variable v) => Located (Expr v) -> String
expr (Located _ e) = case e of
  Var n -> variable n
  Con n -> BC.unpack n
  Lit n -> show n
  Unit -> "()"
  Tuple es -> node ("tuple" : map expr es)
  App f as -> node ("app" : map expr (f : toList as))
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

pat :: (Variable v) => Located (Pattern v) -> String
pat (Located _ p) = case p of
  PWild -> "_"
  PVar n -> variable n
  PLit n -> show n
  PUnit -> "()"
  PCon n [] -> BC.unpack n
  PCon n ps -> node (BC.unpack n : map pat ps)
  PTuple ps -> node ("tuple" : map pat ps)
  POr ps -> node ("or" : map pat (toList ps))
  PAs q n -> node ["as", pat q, var n]

typ :: Located Type -> String
typ (Located _ t) = case t of
  TyVar n -> BC.unpack n
  TyCon n [] -> BC.unpack n
  TyCon n ts -> node (BC.unpack n : map typ ts)
  TyFun a b -> node ["->", typ a, typ b]
  TyTuple ts -> node ("tuple" : map typ ts)
