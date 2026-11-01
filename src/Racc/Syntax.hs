-- | The syntax tree of version 0.1, mirroring GRAMMAR.md.
module Racc.Syntax
  ( Name,
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

data Decl
  = SignatureDecl Signature
  | EquationDecl Equation
  | TypeDecl (Located Name) [Located Name] (NonEmpty Constructor)
  | TypeAlias (Located Name) [Located Name] (Located Type)

data Signature = Signature (Located Name) (Located Type)

-- | One equation; consecutive ones are grouped into a function later.
data Equation = Equation (Located Name) [Located Pattern] Rhs

data Constructor = Constructor (Located Name) [Located Type]

-- | What follows the parameters of an equation, or the pattern of an arm.
data Rhs = Plain Body | Guarded (NonEmpty (Guard, Body))

data Guard = Otherwise | Guard (Located Expr)

data Body = ExprBody (Located Expr) | BlockBody Block

data Block = Block [StateLine] (NonEmpty (Located Stmt))

data StateLine = StateSignature Signature | StateVar (Located Name) Body

data Stmt
  = LocalSignature Signature
  | -- | Also `x = e`, which may turn out to be an assignment.
    LocalEquation Equation
  | Bind (Located Pattern) Body
  | -- | The `if` and `elif` branches, and the `else` branch.
    If (NonEmpty (Located Expr, Block)) (Maybe Block)
  | While (Located Expr) Block
  | For (Located Pattern) (Located Expr) Block
  | Match (Located Expr) (NonEmpty (Located Pattern, Rhs))
  | Return (Maybe (Located Expr))
  | Break (Maybe (Located Integer))
  | Continue (Maybe (Located Integer))
  | Pass
  | ExprStmt (Located Expr)

data Expr
  = Var Name
  | Con Name
  | Lit Integer
  | Unit
  | -- | Two or more elements.
    Tuple [Located Expr]
  | App (Located Expr) (NonEmpty (Located Expr))
  | Unary UnaryOp (Located Expr)
  | Binary BinaryOp (Located Expr) (Located Expr)
  | -- | A chain such as `a < b <= c`.
    Compare (Located Expr) (NonEmpty (CompareOp, Located Expr))
  | -- | The optional last part is the step (`by`).
    Range (Located Expr) RangeOp (Located Expr) (Maybe (Located Expr))
  | IfExpr (Located Expr) (Located Expr) (Located Expr)

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

data Pattern
  = PWild
  | PVar Name
  | PLit Integer
  | PUnit
  | PCon Name [Located Pattern]
  | -- | Two or more elements.
    PTuple [Located Pattern]
  | -- | Two or more alternatives.
    POr (NonEmpty (Located Pattern))
  | PAs (Located Pattern) (Located Name)

data Type
  = TyVar Name
  | TyCon Name [Located Type]
  | TyFun (Located Type) (Located Type)
  | TyUnit
  | -- | Two or more elements.
    TyTuple [Located Type]

-- * Printing as S-expressions

-- | A declaration as one line, such as @(eq f (x) (+ x 1))@.
sexpr :: Located Decl -> String
sexpr (Located _ d) = case d of
  SignatureDecl s -> signature s
  EquationDecl e -> equation e
  TypeDecl n vs cs -> node (["type", name n, node (map name vs)] <> [node (name c : map typ ts) | Constructor c ts <- toList cs])
  TypeAlias n vs t -> node ["alias", name n, node (map name vs), typ t]

node :: [String] -> String
node xs = "(" <> unwords xs <> ")"

name :: Located ByteString -> String
name (Located _ n) = BC.unpack n

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
