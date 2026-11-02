{-# LANGUAGE OverloadedStrings #-}

-- | Name resolution, following the scope rules of DESIGN.md: every variable
-- becomes a reference to its definition. Reports every error it finds, and
-- warns about unused locals.
module Racc.Resolve (Ref (..), prelude, resolve) where

import Control.Monad (foldM, foldM_, guard, unless, when)
import Control.Monad.Trans.State.Strict (State, gets, modify, runState, state)
import Data.Bifunctor (second)
import Data.ByteString.Char8 qualified as BC
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (for_, toList, traverse_)
import Data.Graph (SCC (CyclicSCC), stronglyConnComp)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (groupBy, minimumBy, sort)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, (<|))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Racc.Diagnostic (Diagnostic (Diagnostic), Located (Located), Pos (Pos), Result, Severity (Error, Warning), Span (Span), quote)
import Racc.Lexer (tokenize)
import Racc.Parser (parse)
import Racc.Syntax
  ( Block (Block),
    Body (BlockBody, ExprBody),
    Constructor (Constructor),
    Decl (EquationDecl, SignatureDecl, TypeAlias, TypeDecl),
    Equation (Equation),
    Expr (App, Binary, Compare, Con, IfExpr, Lit, Tuple, Unary, Unit, Var),
    Guard (Guard, Otherwise),
    Name,
    Pattern (PAs, PCon, PLit, POr, PTuple, PUnit, PVar, PWild),
    Range (Range),
    Rhs (Guarded, Plain),
    Signature (Signature),
    StateLine (StateSignature, StateVar),
    Stmt (Bind, Break, Continue, ExprStmt, For, If, LocalEquation, LocalSignature, Match, Pass, Return, While),
    Type (TyCon, TyFun, TyTuple, TyVar),
    Variable (variable),
  )

-- | What a variable refers to: a top-level or built-in definition, or a
-- local one, numbered to tell apart locals with the same name.
data Ref = Global Name | Local Name Int
  deriving stock (Eq, Ord)

instance Variable Ref where
  variable = \case
    Global x -> BC.unpack x
    Local x i -> BC.unpack x <> "." <> show i

-- | The built-ins that Racc can express.
prelude :: [Located (Decl Name)]
prelude = either (error . show) id (parse =<< tokenize "type Bool\n  False\n  True\n")

-- | The built-ins that it can't: types without parameters, and values.
primitiveTypes, primitiveValues :: [Name]
primitiveTypes = ["Int", "Void"]
primitiveValues = ["print"]

data S = S
  { counter :: !Int,
    diagnostics :: [Diagnostic],
    -- | Every local, by number.
    locals :: [(Int, Located Name)],
    used :: !IntSet,
    -- | The top-level names that the current top-level function uses.
    usedGlobals :: Set Name,
    -- | Each top-level function: whether it is a value, its name, and the
    -- top-level names it uses.
    functions :: [(Bool, Located Name, [Name])],
    -- | The smallest depth of the state that the current function uses.
    touched :: !Int,
    -- | Local functions that use enclosing state, with the smallest depth
    -- of that state and their number of parameters.
    stateful :: IntMap (Int, Int),
    -- | Each use of a local, with the number of arguments it is applied to.
    applied :: [(Int, Int, Located Name)]
  }

type R = State S

data Env = Env
  { values :: Map Name Entry,
    -- | Types and constructors, with their number of arguments.
    types :: Map Name Int,
    constructors :: Map Name Int,
    -- | Nesting of the current function: 1 in a top-level function.
    depth :: !Int,
    -- | Loops around the current statement, in the current function.
    loops :: !Int
  }

-- | A variable in scope, the depth of the function that defines it, and
-- whether it is a place.
data Entry = Entry Ref Int Bool

resolve :: [Located (Decl Name)] -> Result [Located (Decl Ref)]
resolve ds = (diags, out <$ guard (all (\(Diagnostic sev _ _) -> sev == Warning) diags))
  where
    (out, st) = runState (program ds) (S 0 [] [] IntSet.empty Set.empty [] maxBound IntMap.empty [])
    diags = diagnostics st <> unused <> stateValues
    stateValues =
      [ Diagnostic Error s (quote x <> " uses the state of an enclosing function, so it can only be called")
      | (i, n, Located s x) <- applied st,
        Just (_, k) <- [IntMap.lookup i (stateful st)],
        n < k
      ]
    unused =
      [ Diagnostic Warning s (quote x <> " is unused")
      | (i, Located s x) <- locals st,
        not (IntSet.member i (used st)),
        not ("_" `BC.isPrefixOf` x)
      ]

program :: [Located (Decl Name)] -> R [Located (Decl Ref)]
program ds = do
  duplicates (map builtin primitiveTypes <> map fst typeHeads)
  duplicates [c | (c, _) <- cons]
  duplicates (map builtin primitiveValues <> fns)
  traverse_ (duplicates . snd) typeHeads
  out <- decls env ds
  fs <- gets functions
  for_ (cycles fs) $ \(Located s x) -> report s ("the value " <> quote x <> " depends on itself")
  for_ (cycles [(True, n, tyCons t) | Located _ (TypeAlias n _ t) <- ds]) $ \(Located s x) ->
    report s ("the type alias " <> quote x <> " refers to itself")
  pure out
  where
    everything = prelude <> ds
    builtin :: Name -> Located Name
    builtin = Located (Span (Pos 1 1) (Pos 1 1))
    typeHeads =
      concatMap (\case Located _ (TypeDecl n vs _) -> [(n, vs)]; Located _ (TypeAlias n vs _) -> [(n, vs)]; _ -> []) everything
    cons = [(c, length ts) | Located _ (TypeDecl _ _ cs) <- everything, Constructor c ts <- toList cs]
    fns = [x | Located _ (EquationDecl (Equation x _ _)) : _ <- groupBy (\a b -> isJust (equationName a) && equationName a == equationName b) everything]
    env =
      Env
        { values = Map.fromList [(x, Entry (Global x) 0 False) | x <- primitiveValues <> [x | Located _ x <- fns]],
          types = Map.fromList (map (,0) primitiveTypes <> [(n, length vs) | (Located _ n, vs) <- typeHeads]),
          constructors = Map.fromList [(c, n) | (Located _ c, n) <- cons],
          depth = 0,
          loops = 0
        }

-- | Reports each name that occurs again, where it does.
duplicates :: [Located Name] -> R ()
duplicates = foldM_ add Set.empty
  where
    add seen (Located s x)
      | Set.member x seen = seen <$ report s (quote x <> " is already defined")
      | otherwise = pure (Set.insert x seen)

-- | The name of an equation with parameters; only those form a function
-- with the equations after them.
equationName :: Located (Decl n) -> Maybe n
equationName = \case
  Located _ (EquationDecl (Equation (Located _ x) (_ : _) _)) -> Just x
  _ -> Nothing

-- | Each cycle of definitions that contains a marked one, as its first
-- marked definition.
cycles :: [(Bool, Located Name, [Name])] -> [Located Name]
cycles ns =
  [ minimumBy (comparing (\(Located s _) -> s)) marked
  | CyclicSCC c <- stronglyConnComp [((m, x), n, deps) | (m, x@(Located _ n), deps) <- nubOrdOn (\(_, Located _ n, _) -> n) ns],
    let marked = [x | (True, x) <- c],
    not (null marked)
  ]

tyCons :: Located Type -> [Name]
tyCons (Located _ t) = case t of
  TyCon c ts -> c : concatMap tyCons ts
  TyFun a b -> tyCons a <> tyCons b
  TyTuple ts -> concatMap tyCons ts
  _ -> []

-- * Declarations

decls :: Env -> [Located (Decl Name)] -> R [Located (Decl Ref)]
decls _ [] = pure []
decls env (Located s d : rest) = case d of
  SignatureDecl (Signature (Located sx x) t) -> do
    checkType env Nothing t
    case rest of
      Located _ (EquationDecl (Equation (Located _ y) _ _)) : _ | y == x -> pure ()
      _ -> report sx (unsigned x)
    (Located s (SignatureDecl (Signature (Located sx (Global x)) t)) :) <$> decls env rest
  EquationDecl e@(Equation lx@(Located _ x) ps _) -> do
    let (more, rest') = span (\o -> not (null ps) && equationName o == Just x) rest
    modify (\st -> st {usedGlobals = Set.empty})
    out <- function env (Global x) (Located s e :| [Located s' e' | Located s' (EquationDecl e') <- more])
    refs <- gets usedGlobals
    modify (\st -> st {functions = (null ps, lx, Set.toList refs) : functions st})
    (map (fmap EquationDecl) (toList out) <>) <$> decls env rest'
  TypeDecl n vs cs -> do
    for_ cs $ \(Constructor _ ts) -> traverse_ (checkType env (Just (n, vs))) ts
    (Located s (TypeDecl n vs cs) :) <$> decls env rest
  TypeAlias n vs t -> do
    checkType env (Just (n, vs)) t
    (Located s (TypeAlias n vs t) :) <$> decls env rest

-- | The equations of a function that `r` refers to; each is a new function
-- scope.
function :: Env -> Ref -> NonEmpty (Located (Equation Name)) -> R (NonEmpty (Located (Equation Ref)))
function env r eqs@(Located _ (Equation _ ps _) :| _) = do
  outer <- gets touched
  modify (\st -> st {touched = maxBound})
  out <- traverse (traverse clause) eqs
  t <- gets touched
  -- A value uses state where it is defined, a function where it is called.
  modify (\st -> st {touched = if null ps then min outer t else outer})
  for_ [i | not (null ps), t <= depth env, Local _ i <- [r]] $ \i ->
    modify (\st -> st {stateful = IntMap.insert i (t, length ps) (stateful st)})
  pure out
  where
    clause (Equation (Located s x) qs body') = do
      when (length qs /= length ps) (report s ("the equations of " <> quote x <> " have different numbers of parameters"))
      (inner, m) <- defineAll env {depth = depth env + 1, loops = 0} (concatMap vars qs)
      Equation (Located s r) <$> traverse (pat env m) qs <*> rhs inner body'

unsigned :: Name -> String
unsigned x = "the signature of " <> quote x <> " must be directly followed by its definition"

-- * Scopes

-- | Defines a new local in the current function, which must not define it
-- already.
define :: Bool -> Env -> Located Name -> R (Env, Ref)
define isPlace env (Located s x) = do
  let taken = maybe False (\(Entry _ d _) -> d == depth env) (Map.lookup x (values env))
  when taken (report s (quote x <> " is already defined"))
  i <- state (\st -> (counter st, st {counter = counter st + 1, locals = [(counter st, Located s x) | not taken] <> locals st}))
  pure (env {values = Map.insert x (Entry (Local x i) (depth env) isPlace) (values env)}, Local x i)

defineAll :: Env -> [Located Name] -> R (Env, Map Name Ref)
defineAll env = foldM (\(e, m) x@(Located _ n) -> second (\r -> Map.insert n r m) <$> define False e x) (env, Map.empty)

-- | The place that a name refers to, if it does.
place :: Env -> Name -> Maybe Ref
place env x = case Map.lookup x (values env) of
  Just (Entry r _ True) -> Just r
  _ -> Nothing

refOf :: Env -> Name -> Ref
refOf env x = maybe (Global x) (\(Entry r _ _) -> r) (Map.lookup x (values env))

use :: Env -> Span -> Name -> R Ref
use env s x = case Map.lookup x (values env) of
  Just (Entry r d isPlace) -> do
    when isPlace (touch d)
    case r of
      Local _ i -> do
        modify (\st -> st {used = IntSet.insert i (used st)})
        gets (IntMap.lookup i . stateful) >>= traverse_ (touch . fst)
      Global n -> modify (\st -> st {usedGlobals = Set.insert n (usedGlobals st)})
    pure r
  Nothing -> Global x <$ report s (quote x <> " is not defined")

-- | Notes that the current function uses state of a function at depth `d`.
touch :: Int -> R ()
touch d = modify (\st -> st {touched = min d (touched st)})

report :: Span -> String -> R ()
report s msg = modify (\st -> st {diagnostics = Diagnostic Error s msg : diagnostics st})

-- * Statements

rhs :: Env -> Rhs Name -> R (Rhs Ref)
rhs env = \case
  Plain b -> Plain <$> body env b
  Guarded gs -> Guarded <$> traverse (\(g, b) -> (,) <$> guard' g <*> body env b) gs
  where
    guard' = \case
      Otherwise -> pure Otherwise
      Guard e -> Guard <$> expr env e

body :: Env -> Body Name -> R (Body Ref)
body env = \case
  ExprBody e -> ExprBody <$> expr env e
  BlockBody b -> BlockBody <$> block env b

block :: Env -> Block Name -> R (Block Ref)
block env (Block sv ss) = do
  (env', sv') <- stateLines env sv
  Block sv' <$> stmts env' ss

stateLines :: Env -> [StateLine Name] -> R (Env, [StateLine Ref])
stateLines env = \case
  [] -> pure (env, [])
  StateSignature (Signature (Located sx x) t) : rest -> do
    checkType env Nothing t
    case rest of
      StateVar (Located _ y) _ : _ | y == x -> pure ()
      _ -> report sx (unsigned x)
    (env', out) <- stateLines env rest
    pure (env', StateSignature (Signature (Located sx (refOf env' x)) t) : out)
  StateVar x b : rest -> do
    b' <- body env b
    (env', r) <- define True env x
    second (StateVar (r <$ x) b' :) <$> stateLines env' rest

stmts :: Env -> NonEmpty (Located (Stmt Name)) -> R (NonEmpty (Located (Stmt Ref)))
stmts env (s :| ss) = do
  (env', out, rest) <- stmt env s ss
  maybe (pure out) (fmap (out <>) . stmts env') (nonEmpty rest)

-- | A statement and the ones after it that belong to it (a function's other
-- equations, or the definition after a signature), the scope after them,
-- and the statements left.
stmt ::
  Env ->
  Located (Stmt Name) ->
  [Located (Stmt Name)] ->
  R (Env, NonEmpty (Located (Stmt Ref)), [Located (Stmt Name)])
stmt env (Located s st) rest = case st of
  LocalSignature (Signature (Located sx x) t) -> do
    checkType env Nothing t
    let sig r = Located s (LocalSignature (Signature (Located sx r) t))
    case rest of
      d@(Located _ (LocalEquation (Equation (Located _ y) ps _))) : rest'
        | y == x && (not (null ps) || isNothing (place env x)) -> do
            (env', out, rest'') <- stmt env d rest'
            pure (env', sig (refOf env' x) <| out, rest'')
      _ -> (env, sig (Global x) :| [], rest) <$ report sx (unsigned x)
  LocalEquation e@(Equation lx@(Located sx x) ps r) -> case Map.lookup x (values env) of
    Just (Entry v d isPlace) | null ps && (isPlace || d == depth env) -> do
      when isPlace (touch d)
      unless isPlace (report sx (quote x <> " is not a state variable, so it can't be assigned"))
      one (LocalEquation . Equation (Located sx v) [] <$> rhs env {loops = 0} r)
    _ -> do
      let (more, rest') = span (\o -> not (null ps) && stmtName o == Just x) rest
      (env', v) <- define False env lx
      out <- function (if null ps then env else env') v (Located s e :| [Located s' e' | Located s' (LocalEquation e') <- more])
      -- Calls from its own body don't make it used.
      for_ [i | Local _ i <- [v]] $ \i -> modify (\t -> t {used = IntSet.delete i (used t)})
      pure (env', fmap LocalEquation <$> out, rest')
  Bind p@(Located sp _) b -> do
    -- A block here is a body of its own, which no loop is around.
    b' <- body env {loops = 0} b
    let xs = vars p
        places = Map.fromList [(x, r) | Located _ x <- xs, Just r <- [place env x]]
        new = [x | x@(Located _ n) <- xs, Map.notMember n places]
    when (not (Map.null places) && not (null new)) $
      report sp "a pattern must either assign state variables or define new names, not both"
    for_ [d | Located _ x <- xs, Just (Entry _ d True) <- [Map.lookup x (values env)]] touch
    (env', m) <- defineAll env new
    p' <- pat env (places <> m) p
    pure (env', Located s (Bind p' b') :| [], rest)
  If bs e -> one (If <$> traverse (\(c, b) -> (,) <$> expr env c <*> block env b) bs <*> traverse (block env) e)
  While c b -> one (While <$> expr env c <*> block (inLoop env) b)
  For p (Range a op b c) bl -> one $ do
    r <- Range <$> expr env a <*> pure op <*> expr env b <*> traverse (expr env) c
    (env', m) <- defineAll env (vars p)
    For <$> pat env m p <*> pure r <*> block (inLoop env') bl
  Match e arms -> one (Match <$> expr env e <*> traverse arm arms)
  Return e -> one (Return <$> traverse (expr env) e)
  Break n -> one (Break n <$ jump "break" n)
  Continue n -> one (Continue n <$ jump "continue" n)
  Pass -> one (pure Pass)
  ExprStmt e -> one (ExprStmt <$> expr env e)
  where
    one = fmap (\x -> (env, Located s x :| [], rest))
    inLoop e = e {loops = loops e + 1}
    arm (p, r) = do
      (env', m) <- defineAll env (vars p)
      (,) <$> pat env m p <*> rhs env' r
    jump kw = \case
      Nothing -> when (loops env == 0) (report s (quote kw <> " must be inside a loop"))
      Just (Located sn n)
        | n < 2 -> report sn ("the loop count after " <> quote kw <> " must be at least 2")
        | n > toInteger (loops env) ->
            report sn (quote (kw <> " " <> BC.pack (show n)) <> " needs " <> show n <> " loops around it, but there " <> (if loops env == 1 then "is " else "are ") <> show (loops env))
        | otherwise -> pure ()

stmtName :: Located (Stmt n) -> Maybe n
stmtName = \case
  Located _ (LocalEquation (Equation (Located _ x) (_ : _) _)) -> Just x
  _ -> Nothing

count :: Int -> String -> String
count n w = show n <> " " <> w <> if n == 1 then "" else "s"

-- * Expressions, patterns, and types

expr :: Env -> Located (Expr Name) -> R (Located (Expr Ref))
expr env (Located s e) =
  Located s <$> case e of
    Var x -> Var <$> local 0 s x
    Con c -> Con c <$ constructor env s c Nothing
    Lit n -> pure (Lit n)
    Unit -> pure Unit
    Tuple es -> Tuple <$> traverse go es
    App (Located sf (Var x)) as -> App . Located sf . Var <$> local (length as) sf x <*> traverse go as
    App f as -> App <$> go f <*> traverse go as
    Unary op a -> Unary op <$> go a
    Binary op a b -> Binary op <$> go a <*> go b
    Compare a rest -> Compare <$> go a <*> traverse (traverse go) rest
    IfExpr c a b -> IfExpr <$> go c <*> go a <*> go b
  where
    go = expr env
    local n s' x = do
      r <- use env s' x
      for_ [i | Local _ i <- [r]] $ \i -> modify (\st -> st {applied = (i, n, Located s' x) : applied st})
      pure r

-- | The variables a pattern binds, where they occur.
vars :: Located (Pattern Name) -> [Located Name]
vars (Located s p) = case p of
  PVar x -> [Located s x]
  PCon _ ps -> concatMap vars ps
  PTuple ps -> concatMap vars ps
  POr (q :| _) -> vars q
  PAs q x -> vars q <> [x]
  _ -> []

-- | A pattern whose variables refer to what they map to.
pat :: Env -> Map Name Ref -> Located (Pattern Name) -> R (Located (Pattern Ref))
pat env m (Located s p) =
  Located s <$> case p of
    PWild -> pure PWild
    PVar x -> pure (PVar (ref x))
    PLit n -> pure (PLit n)
    PUnit -> pure PUnit
    PCon c ps -> constructor env s c (Just (length ps)) >> PCon c <$> traverse go ps
    PTuple ps -> PTuple <$> traverse go ps
    POr qs@(q :| _) -> do
      unless (all ((== names q) . names) qs) (report s "each alternative must bind the same names")
      POr <$> traverse go qs
    PAs q x -> PAs <$> go q <*> pure (ref <$> x)
  where
    go = pat env m
    ref x = Map.findWithDefault (Global x) x m
    names q = sort [x | Located _ x <- vars q]

-- | Checks a constructor, and its number of arguments if given.
constructor :: Env -> Span -> Name -> Maybe Int -> R ()
constructor env s c given = case Map.lookup c (constructors env) of
  Nothing -> report s (quote c <> " is not a constructor")
  Just n -> for_ given $ \k -> when (k /= n) (report s (quote c <> " takes " <> count n "argument" <> " but has " <> show k))

-- | Checks a type; in a type declaration, its variables must be parameters
-- of the declared type.
checkType :: Env -> Maybe (Located Name, [Located Name]) -> Located Type -> R ()
checkType env params (Located s t) = case t of
  TyVar a -> for_ params $ \(Located _ n, vs) ->
    unless (a `elem` [v | Located _ v <- vs]) (report s (quote a <> " is not a parameter of " <> quote n))
  TyCon c ts -> do
    case Map.lookup c (types env) of
      Nothing -> report s (quote c <> " is not a type")
      Just n -> when (n /= length ts) (report s (quote c <> " takes " <> count n "type argument" <> " but has " <> show (length ts)))
    traverse_ go ts
  TyFun a b -> go a >> go b
  TyTuple ts -> traverse_ go ts
  where
    go = checkType env params
