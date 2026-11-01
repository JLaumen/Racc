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
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Racc.Diagnostic (Located)

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
