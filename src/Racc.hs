{-# LANGUAGE OverloadedStrings #-}

module Racc (compile, parseTree, resolveTree) where

import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Racc.Diagnostic (Diagnostic (Diagnostic), Located (Located), Pos (Pos), Result, Severity (Error), Span (Span))
import Racc.Lexer (tokenize)
import Racc.Parser (parse)
import Racc.Resolve (resolve)
import Racc.Syntax (Decl (EquationDecl), Equation (Equation), Name, Variable, sexpr)

-- | Compile Racc source to C.
compile :: ByteString -> Result String
compile = phases $ \ds ->
  let (diags, p) = resolve ds
      atStart = Diagnostic Error (Span (Pos 1 1) (Pos 1 1))
      hasMain = not (null [() | Located _ (EquationDecl (Equation (Located _ "main") _ _)) <- ds])
   in ( diags
          <> [atStart (if hasMain then "code generation is not implemented" else "there is no `main`") | isJust p],
        Nothing
      )

-- | The syntax tree of Racc source, one declaration per line.
parseTree :: ByteString -> Result String
parseTree = phases (\ds -> ([], Just (tree ds)))

-- | The syntax tree after name resolution.
resolveTree :: ByteString -> Result String
resolveTree = phases (second (fmap tree) . resolve)

-- | Parses, then runs the later phases if that succeeded; sorts the
-- diagnostics by position.
phases :: ([Located (Decl Name)] -> Result a) -> ByteString -> Result a
phases later src = either (\d -> ([d], Nothing)) (first (sortOn (\(Diagnostic _ s _) -> s)) . later) (parse =<< tokenize src)

tree :: (Variable v) => [Located (Decl v)] -> String
tree = unlines . map sexpr
