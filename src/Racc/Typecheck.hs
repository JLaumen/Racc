{-# LANGUAGE OverloadedStrings #-}

-- | Type inference, following the type rules of DESIGN.md: Hindley-Milner,
-- with levels to tell which variables to generalize. Reports every error it
-- finds, and warns about discarded values.
module Racc.Typecheck (typecheck) where

import Control.Monad (foldM, foldM_, guard, unless, void, when, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, gets, modify, runState, state)
import Data.ByteString.Char8 qualified as BC
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Foldable (for_, toList, traverse_)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Data.Traversable (for)
import Racc.Diagnostic (Diagnostic (Diagnostic), Located (Located), Result, Severity (Error, Warning), Span, quote)
import Racc.Resolve (Ref (Global, Local), prelude)
import Racc.Syntax
  ( BinaryOp (And, Or),
    Block (Block),
    Body (BlockBody, ExprBody),
    CompareOp (Equal, NotEqual),
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
    UnaryOp (Complement, Neg, Not),
    Variable (variable),
  )

-- | A type; a rigid variable comes from a signature and stands for any type.
data Ty = TVar Int | TRigid Name Int | TCon Name [Ty] | TFun Ty Ty | TTuple [Ty]

-- | A type generalized over the variables with these numbers.
data Scheme = Forall [Int] Ty

data S = S
  { counter :: !Int,
    subst :: IntMap Ty,
    -- | The level of each variable: the number of definitions around the
    -- one it belongs to.
    levels :: IntMap Int,
    -- | Variables that must be `Int` or `Bool`, with what requires it.
    printable :: IntMap (Span, String),
    diagnostics :: [Diagnostic],
    -- | Statements whose values are discarded.
    discarded :: [(Span, Ty)],
    -- | Where bodies can end, with their result types, until the end of
    -- their definition group.
    endings :: [(Span, Ty)],
    -- | Loops that a `break` leaves.
    broken :: IntSet,
    -- | Each definition and parameter, with its top-level definition.
    typed :: [(Name, Ref, Ty)],
    -- | Each use of a generalized definition: its type there, and what its
    -- type variables became.
    uses :: [(Span, Ref, Ty, [(Int, Ty)])]
  }

type T = State S

data Env = Env
  { values :: Map Ref Scheme,
    constructors :: Map Name Scheme,
    aliases :: Map Name ([Name], Located Type),
    -- | The type variables of the signatures around.
    tyVars :: Map Name Ty,
    level :: !Int,
    -- | The result type of the current function.
    result :: Ty,
    -- | The loops around, innermost first.
    loops :: [Int],
    -- | The top-level definition being checked.
    owner :: Name
  }

-- | A function or value with its signature, if any.
data Def = Def (Maybe (Located Type)) (NonEmpty (Located (Equation Ref)))

data Item = ISig (Located Type) | IEq (Located (Equation Ref)) | IStmt (Located (Stmt Ref))

data Unit = UDef Def | UStmt (Located (Stmt Ref))

-- | Checks a program; the result lists the type of each definition, with
-- those of its parameters and locals below it.
typecheck :: [Located (Decl Ref)] -> Result String
typecheck ds = (diagnostics st, out <$ guard (not (any isError (diagnostics st))))
  where
    (out, st) = runState (program ds) (S 0 IntMap.empty IntMap.empty IntMap.empty [] [] [] IntSet.empty [] [])

program :: [Located (Decl Ref)] -> T String
program ds = do
  let typeDecls = [(n, vs, cs) | Located _ (TypeDecl (Located _ n) vs cs) <- prelude] <> [(n, vs, cs) | Located _ (TypeDecl (Located _ n) vs cs) <- ds]
      as = Map.fromList [(n, ([v | Located _ v <- vs], t)) | Located _ (TypeAlias (Located _ n) vs t) <- ds]
  cons <- fmap concat . for typeDecls $ \(n, vs, cs) -> do
    is <- traverse (const newId) vs
    let m = Map.fromList (zip [v | Located _ v <- vs] (map TVar is))
    pure [(c, Forall is (foldr (TFun . convert as m) (TCon n (map TVar is)) ts)) | Constructor (Located _ c) ts <- toList cs]
  let env0 = Env Map.empty (Map.fromList cons) as Map.empty 0 voidTy [] ""
      defs = [d | UDef d <- units (concatMap item ds)]
      item = \case
        Located _ (SignatureDecl (Signature _ t)) -> [ISig t]
        Located s (EquationDecl e) -> [IEq (Located s e)]
        _ -> []
  sigs <- for defs $ \d@(Def t _) -> (d,) <$> traverse (signature env0 1) t
  let unsigned = Set.fromList [defRef d | (d, Nothing) <- sigs]
      deps (Def _ eqs) = [g | Located _ e <- toList eqs, g@(Global _) <- toList e, Set.member g unsigned]
      env1 = env0 {values = Map.fromList [(defRef d, s) | (d, Just (s, _)) <- sigs]}
  foldM_ (\e -> defGroup e . flattenSCC) env1 (stronglyConnComp [(x, defRef d, deps d) | x@(d, _) <- sigs])
  ok <- gets (not . any isError . diagnostics)
  when ok growing
  ws <- gets discarded
  for_ ws $ \(s, t) ->
    zonk t >>= \case
      TCon "Void" [] -> pure ()
      u -> diagnose Warning s ("the value of this statement, of type " <> concatMap quoted (pretty [u]) <> ", is discarded")
  byOwner <- gets (Map.fromListWith (<>) . map (\(o, r, t) -> (o, [(r, t)])) . typed)
  fmap (unlines . concat) . for defs $ \d -> do
    let entries = sortOn (\(r, _) -> case r of Global _ -> -1; Local _ i -> i) (Map.findWithDefault [] (refName (defRef d)) byOwner)
    zs <- traverse (zonk . snd) entries
    pure [(case r of Global _ -> ""; Local _ _ -> "  ") <> variable r <> " : " <> s | ((r, _), s) <- zip entries (pretty zs)]

-- | Reports uses that make the types of a recursion grow each time around
-- it, which would need infinitely many types. Each use makes an edge from
-- each type variable in what a type variable becomes to that one; it grows
-- unless it becomes just that variable. A growing edge in a cycle is an
-- error.
growing :: T ()
growing = do
  us <- gets uses >>= traverse (\(s, r, t, m) -> (s,r,,) <$> zonk t <*> traverse (traverse zonk) m)
  let edges = [(a, b, (s, (r, t)), varId u /= [a]) | (s, r, t, m) <- us, (b, u) <- m, a <- concatMap varId (vars u)]
      next = IntMap.fromListWith (<>) [(a, [b]) | (a, b, _, _) <- edges]
      comp = IntMap.fromList [(a, k) | (k, c) <- zip [0 :: Int ..] (stronglyConnComp [(a, a, bs) | (a, bs) <- IntMap.toList next]), a <- flattenSCC c]
  for_ (nubOrdOn fst [use | (a, b, use, True) <- edges, IntMap.lookup a comp == IntMap.lookup b comp]) $ \(s, (r, t)) ->
    report s (quote (refName r) <> " is used here at " <> concatMap quoted (pretty [t]) <> ", so its type grows with each recursive call")

-- | Groups signatures and equations into definitions.
units :: [Item] -> [Unit]
units = \case
  ISig t : IEq e : rest -> def (Just t) e rest
  IEq e : rest -> def Nothing e rest
  IStmt s : rest -> UStmt s : units rest
  ISig _ : rest -> units rest
  [] -> []
  where
    def t e@(Located _ (Equation (Located _ r) ps _)) rest =
      let same = \case
            IEq (Located _ (Equation (Located _ r') _ _)) -> not (null ps) && r' == r
            _ -> False
          (more, rest') = span same rest
       in UDef (Def t (e :| [o | IEq o <- more])) : units rest'

defRef :: Def -> Ref
defRef (Def _ (Located _ (Equation (Located _ r) _ _) :| _)) = r

refName :: Ref -> Name
refName = \case
  Global n -> n
  Local n _ -> n

-- * Definitions

-- | Checks definitions that may call each other, one level deeper than the
-- scope, and adds them to it, generalized.
defGroup :: Env -> [(Def, Maybe (Scheme, Map Name Ty))] -> T Env
defGroup env ds = do
  let inner = env {level = level env + 1}
  ds' <- for ds $ \case
    (d, Just (s@(Forall _ t), vs)) -> pure (d, s, t, vs)
    (d, Nothing) -> fresh inner >>= \t -> pure (d, Forall [] t, t, tyVars env)
  let with ss = env {values = Map.fromList ss <> values env}
      rec = with [(defRef d, s) | (d, s, _, _) <- ds']
      own d = if level env == 0 then refName (defRef d) else owner env
  outer <- gets endings
  modify (\st -> st {endings = []})
  for_ ds' $ \(d, _, t, vs) -> definition rec {level = level env + 1, tyVars = vs, owner = own d} t d
  -- Reaching the end of a body gives `()`.
  es <- gets endings
  modify (\st -> st {endings = outer})
  for_ es $ \(s, t) ->
    zonk t >>= \case
      TCon "Void" [] -> pure ()
      TVar _ -> unify s voidTy t
      _ -> report s "`return` is missing: the body can end after this statement"
  fmap with . for ds' $ \(d, _, t, _) -> do
    modify (\st -> st {typed = (own d, defRef d, t) : typed st})
    (defRef d,) <$> generalize env t

definition :: Env -> Ty -> Def -> T ()
definition env t (Def _ eqs) = for_ eqs $ \(Located _ (Equation (Located s r) ps rh)) -> do
  when (r == Global "main") (unify s voidTy t)
  args <- traverse (const (fresh env)) ps
  res <- fresh env
  unify s t (foldr TFun res args)
  env' <- foldM (\e (p, a) -> pat e a p) env {result = res, loops = []} (zip ps args)
  void (rhs env' (Just res) rh)

-- | The type of a signature at a level, with the type variables in scope;
-- new ones are rigid.
signature :: Env -> Int -> Located Type -> T (Scheme, Map Name Ty)
signature env l t = do
  let new = nubOrd [a | Located _ a <- typeVars t, Map.notMember a (tyVars env)]
  is <- traverse (const (newVar l)) new
  let vs = Map.fromList (zip new (zipWith TRigid new is)) <> tyVars env
  pure (Forall is (convert (aliases env) vs t), vs)

typeVars :: Located Type -> [Located Name]
typeVars (Located s t) = case t of
  TyVar a -> [Located s a]
  TyCon _ ts -> concatMap typeVars ts
  TyFun a b -> typeVars a <> typeVars b
  TyTuple ts -> concatMap typeVars ts

-- | A written type, with type aliases expanded.
convert :: Map Name ([Name], Located Type) -> Map Name Ty -> Located Type -> Ty
convert as m (Located _ t) = case t of
  TyVar a -> Map.findWithDefault (TCon a []) a m
  TyCon c ts -> case Map.lookup c as of
    Just (vs, u) -> convert as (Map.fromList (zip vs (map go ts))) u
    Nothing -> TCon c (map go ts)
  TyFun a b -> TFun (go a) (go b)
  TyTuple ts -> TTuple (map go ts)
  where
    go = convert as m

generalize :: Env -> Ty -> T Scheme
generalize env t = do
  -- Settles the requirements of this definition; those on variables of
  -- the definitions around it wait for those.
  ps <- gets printable
  for_ (IntMap.toList ps) $ \(i, (s, what)) -> do
    let settle msg = do
          traverse_ (report s . (what <>)) msg
          modify (\st -> st {printable = IntMap.delete i (printable st)})
    zonk (TVar i) >>= \case
      TVar j -> levelOf j >>= \l -> when (l > level env) (settle (Just " needs `Int` or `Bool`, but the type here is not known; add a signature"))
      TCon c [] | c `elem` ["Int", "Bool"] -> settle Nothing
      u -> settle (Just (" needs `Int` or `Bool`, not " <> concatMap quoted (pretty [u])))
  t' <- zonk t
  ls <- gets levels
  pure (Forall (nubOrd [i | v <- vars t', i <- varId v, IntMap.findWithDefault 0 i ls > level env]) t')

-- | A type of a scheme, with what its type variables became.
instantiate :: Env -> Scheme -> T (Ty, [(Int, Ty)])
instantiate env (Forall is t) = do
  m <- IntMap.fromList <$> traverse (\i -> (i,) <$> fresh env) is
  let go = \case
        u@(TVar i) -> IntMap.findWithDefault u i m
        u@(TRigid _ i) -> IntMap.findWithDefault u i m
        TCon c as -> TCon c (map go as)
        TFun a b -> TFun (go a) (go b)
        TTuple as -> TTuple (map go as)
  pure (go t, IntMap.toList m)

-- * Statements

-- | Checks a right-hand side. Given its value's type, a one-line body is
-- that value, and a block is a body of its own that gives it with `return`;
-- otherwise its value is discarded. Tells whether it can finish.
rhs :: Env -> Maybe Ty -> Rhs Ref -> T Bool
rhs env t = \case
  Plain b -> body env t b
  Guarded gs -> fmap or . for (toList gs) $ \(g, b) -> do
    case g of
      Guard e -> expr env boolTy e
      Otherwise -> pure ()
    body env t b

body :: Env -> Maybe Ty -> Body Ref -> T Bool
body env m = \case
  ExprBody e@(Located s _) -> True <$ (value env s m >>= \t -> expr env t e)
  BlockBody b -> case m of
    Nothing -> isJust <$> block env b
    Just t -> True <$ (block env {result = t, loops = []} b >>= traverse_ (\s -> modify (\st -> st {endings = (s, t) : endings st})))

-- | Checks a block; gives the span of its last statement if the block can
-- end after it.
block :: Env -> Block Ref -> T (Maybe Span)
block env (Block sv ss) = do
  env' <- stateLines env sv
  let go e = \case
        [u] -> (\(_, f) -> unitSpan u <$ guard f) <$> unit e u
        u : us -> unit e u >>= \(e', _) -> go e' us
        [] -> pure Nothing
  go env' (units (map item (toList ss)))
  where
    item = \case
      Located _ (LocalSignature (Signature _ t)) -> ISig t
      Located s (LocalEquation e) -> IEq (Located s e)
      s -> IStmt s
    unitSpan = \case
      UDef (Def _ (Located s _ :| _)) -> s
      UStmt (Located s _) -> s

stateLines :: Env -> [StateLine Ref] -> T Env
stateLines env = \case
  StateSignature (Signature _ t) : StateVar x b : rest -> do
    for_ [v | v@(Located _ a) <- typeVars t, Map.notMember a (tyVars env)] $ \(Located s a) ->
      report s (quote a <> " is not in scope; a state variable can only use the type variables of its function's signature")
    signature env (level env) t >>= \(Forall _ u, _) -> var u x b rest
  StateVar x b : rest -> fresh env >>= \u -> var u x b rest
  _ : rest -> stateLines env rest
  [] -> pure env
  where
    var u (Located _ r) b rest = body env (Just u) b >> bind env r u >>= \e -> stateLines e rest

-- | Checks a statement, or a local definition; gives the scope after it,
-- and whether it can finish.
unit :: Env -> Unit -> T (Env, Bool)
unit env = \case
  UDef d@(Def sig (Located _ (Equation (Located _ r) ps rh) :| _))
    | isNothing sig, null ps, Just (Forall _ t) <- Map.lookup r (values env) -> (env, True) <$ rhs env (Just t) rh
    | otherwise -> do
        sig' <- traverse (signature env (level env + 1)) sig
        (,True) <$> defGroup env [(d, sig')]
  UStmt (Located s st) -> case st of
    Bind p b -> do
      t <- fresh env
      _ <- body env (Just t) b
      (,True) <$> pat env t p
    If bs e -> do
      fs <- for (toList bs) $ \(c, b) -> expr env boolTy c >> isJust <$> block env b
      fe <- traverse (fmap isJust . block env) e
      pure (env, maybe True (|| or fs) fe)
    While c b -> do
      i <- newId
      expr env boolTy c
      _ <- block env {loops = i : loops env} b
      left <- gets (IntSet.member i . broken)
      let forever = case c of
            Located _ (Con "True") -> not left
            _ -> False
      pure (env, not forever)
    For p (Range a _ b c) bl -> do
      traverse_ (expr env intTy) (a : b : toList c)
      i <- newId
      env' <- pat env intTy p
      _ <- block env' {loops = i : loops env} bl
      pure (env, True)
    Match e arms -> do
      u <- fresh env
      expr env u e
      fs <- for (toList arms) $ \(p, r) -> pat env u p >>= \env' -> rhs env' Nothing r
      pure (env, or fs)
    Return e -> (env, False) <$ maybe (unify s (result env) voidTy) (expr env (result env)) e
    Break n -> do
      let k = maybe 1 (\(Located _ x) -> fromInteger x) n
      for_ (take 1 (drop (k - 1) (loops env))) $ \i -> modify (\st' -> st' {broken = IntSet.insert i (broken st')})
      pure (env, False)
    Continue _ -> pure (env, False)
    Pass -> pure (env, True)
    ExprStmt e -> value env s Nothing >>= \t -> (env, True) <$ expr env t e
    -- Grouped into definitions by `units`.
    LocalSignature _ -> pure (env, True)
    LocalEquation _ -> pure (env, True)

-- | The type of a one-line body's value: the given one, or one that is
-- discarded.
value :: Env -> Span -> Maybe Ty -> T Ty
value env s = maybe (fresh env >>= \t -> t <$ modify (\st -> st {discarded = (s, t) : discarded st})) pure

-- * Expressions and patterns

expr :: Env -> Ty -> Located (Expr Ref) -> T ()
expr env t (Located s e) = case e of
  Var (Global "print") -> do
    a <- printableVar env s "`print`"
    unify s t (TFun a voidTy)
  Var r -> do
    (u, m) <- instantiate env (values env Map.! r)
    unless (null m) (modify (\st -> st {uses = (s, r, u, m) : uses st}))
    unify s t u
  Con c -> instantiate env (constructors env Map.! c) >>= unify s t . fst
  Lit _ -> unify s t intTy
  Unit -> unify s t voidTy
  Tuple es -> do
    ts <- traverse (const (fresh env)) es
    unify s t (TTuple ts)
    zipWithM_ (expr env) ts es
  App f as -> do
    ts <- traverse (const (fresh env)) (toList as)
    expr env (foldr TFun t ts) f
    zipWithM_ (expr env) ts (toList as)
  Unary op a -> do
    let u = case op of
          Not -> boolTy
          Neg -> intTy
          Complement -> intTy
    unify s t u >> expr env u a
  Binary op a b -> do
    let u = case op of
          Or -> boolTy
          And -> boolTy
          _ -> intTy
    unify s t u >> expr env u a >> expr env u b
  Compare a rest -> do
    unify s t boolTy
    let equality :: CompareOp -> Maybe String
        equality = \case
          Equal -> Just "`==`"
          NotEqual -> Just "`!=`"
          _ -> Nothing
    -- Ordering needs `Int`; equality alone, `Int` or `Bool`.
    u <- case traverse (equality . fst) (toList rest) of
      Just (what : _) -> printableVar env s what
      _ -> pure intTy
    traverse_ (expr env u) (a : map snd (toList rest))
  IfExpr c a b -> expr env boolTy c >> expr env t a >> expr env t b

-- | Checks a pattern against the type of the value it matches; gives the
-- scope with its variables.
pat :: Env -> Ty -> Located (Pattern Ref) -> T Env
pat env t (Located s p) = case p of
  PWild -> pure env
  PVar r -> var env r
  PLit _ -> env <$ unify s t intTy
  PUnit -> env <$ unify s t voidTy
  PCon c ps -> do
    (u, _) <- instantiate env (constructors env Map.! c)
    let (args, res) = split ps u
    unify s t res
    foldM (\e (q, a) -> pat e a q) env args
  PTuple ps -> do
    ts <- traverse (const (fresh env)) ps
    unify s t (TTuple ts)
    foldM (\e (q, a) -> pat e a q) env (zip ps ts)
  POr (q :| qs) -> pat env t q >>= \e -> foldM (`pat` t) e qs
  PAs q (Located _ r) -> pat env t q >>= \e -> var e r
  where
    -- A variable bound before, by another alternative or as a place.
    var e r = case Map.lookup r (values e) of
      Just (Forall _ u) -> e <$ unify s u t
      Nothing -> bind e r t

-- | Pairs arguments with the parameter types of a function type, and gives
-- the result type after them.
split :: [a] -> Ty -> ([(a, Ty)], Ty)
split (q : qs) (TFun a b) = let (args, res) = split qs b in ((q, a) : args, res)
split _ res = ([], res)

-- | Adds a parameter, pattern variable, or state variable to the scope.
bind :: Env -> Ref -> Ty -> T Env
bind env r t = do
  modify (\st -> st {typed = (owner env, r, t) : typed st})
  pure env {values = Map.insert r (Forall [] t) (values env)}

-- * Unification

data Problem = Clash | Occurs

-- | Makes the found type equal to the expected one, or reports where they
-- differ.
unify :: Span -> Ty -> Ty -> T ()
unify s expected found =
  runExceptT (go expected found) >>= \case
    Right () -> pure ()
    Left problem -> do
      ts <- map quoted . pretty <$> traverse zonk [expected, found]
      report s $
        concat (zipWith (<>) ["expected ", ", found "] ts) <> case problem of
          Occurs -> " (a type can't contain itself)"
          Clash -> ""
  where
    go :: Ty -> Ty -> ExceptT Problem T ()
    go a b = do
      a' <- lift (shallow a)
      b' <- lift (shallow b)
      case (a', b') of
        (TVar i, TVar j) | i == j -> pure ()
        (TVar i, u) -> bindVar i u
        (u, TVar i) -> bindVar i u
        (TRigid _ i, TRigid _ j) | i == j -> pure ()
        (TCon c as, TCon d bs) | c == d -> zipWithM_ go as bs
        (TFun a1 b1, TFun a2 b2) -> go a1 a2 >> go b1 b2
        (TTuple as, TTuple bs) | length as == length bs -> zipWithM_ go as bs
        _ -> throwE Clash
    bindVar i u = do
      u' <- lift (zonk u)
      let vs = vars u'
      when (or [j == i | TVar j <- vs]) (throwE Occurs)
      l <- lift (levelOf i)
      ls <- lift (gets levels)
      -- A rigid variable may not escape its signature.
      when (or [IntMap.findWithDefault 0 j ls > l | TRigid _ j <- vs]) (throwE Clash)
      lift $ modify (\st -> st {subst = IntMap.insert i u' (subst st), levels = foldr (IntMap.adjust (min l)) (levels st) [j | TVar j <- vs]})

shallow :: Ty -> T Ty
shallow = \case
  t@(TVar i) -> gets (IntMap.lookup i . subst) >>= maybe (pure t) shallow
  t -> pure t

zonk :: Ty -> T Ty
zonk t =
  shallow t >>= \case
    TCon c as -> TCon c <$> traverse zonk as
    TFun a b -> TFun <$> zonk a <*> zonk b
    TTuple as -> TTuple <$> traverse zonk as
    u -> pure u

-- | The variables in a type, rigid or not.
vars :: Ty -> [Ty]
vars = \case
  TCon _ as -> concatMap vars as
  TFun a b -> vars a <> vars b
  TTuple as -> concatMap vars as
  t -> [t]

varId :: Ty -> [Int]
varId = \case
  TVar i -> [i]
  TRigid _ i -> [i]
  _ -> []

-- | Types as text, with their variables named together: rigid ones
-- by their names, the others a, b, ... skipping those.
pretty :: [Ty] -> [String]
pretty ts = map (go (0 :: Int)) ts
  where
    vs = concatMap vars ts
    taken = [BC.unpack n | TRigid n _ <- vs]
    supply = filter (`notElem` taken) ([1 ..] >>= \k -> mapM (const ['a' .. 'z']) [1 .. k :: Int])
    names = IntMap.fromList (zip (nubOrd [i | TVar i <- vs]) supply)
    go p = \case
      TVar i -> IntMap.findWithDefault "?" i names
      TRigid n _ -> BC.unpack n
      TCon c [] -> BC.unpack c
      TCon c as -> paren (p > 1) (unwords (BC.unpack c : map (go 2) as))
      TFun a b -> paren (p > 0) (go 1 a <> " -> " <> go 0 b)
      TTuple as -> "(" <> intercalate ", " (map (go 0) as) <> ")"
    paren :: Bool -> String -> String
    paren b x = if b then "(" <> x <> ")" else x

quoted :: String -> String
quoted x = "`" <> x <> "`"

-- * State

newId :: T Int
newId = state (\st -> (counter st, st {counter = counter st + 1}))

newVar :: Int -> T Int
newVar l = newId >>= \i -> i <$ modify (\st -> st {levels = IntMap.insert i l (levels st)})

fresh :: Env -> T Ty
fresh env = TVar <$> newVar (level env)

-- | A new variable that must be `Int` or `Bool`, for what requires it.
printableVar :: Env -> Span -> String -> T Ty
printableVar env s what = do
  i <- newVar (level env)
  modify (\st -> st {printable = IntMap.insert i (s, what) (printable st)})
  pure (TVar i)

levelOf :: Int -> T Int
levelOf i = gets (IntMap.findWithDefault 0 i . levels)

isError :: Diagnostic -> Bool
isError (Diagnostic sev _ _) = sev == Error

report :: Span -> String -> T ()
report = diagnose Error

diagnose :: Severity -> Span -> String -> T ()
diagnose sev s msg = modify (\st -> st {diagnostics = Diagnostic sev s msg : diagnostics st})

intTy, boolTy, voidTy :: Ty
intTy = TCon "Int" []
boolTy = TCon "Bool" []
voidTy = TCon "Void" []
