module Racc (compile, parseTree) where

import Data.ByteString (ByteString)
import Racc.Diagnostic (Diagnostic (Diagnostic), Located, Pos (Pos), Span (Span))
import Racc.Lexer (tokenize)
import Racc.Parser (parse)
import Racc.Syntax (Decl, sexpr)

-- | Compile Racc source to C.
compile :: ByteString -> Either Diagnostic String
compile src = do
  _ <- syntax src
  Left (Diagnostic (Span (Pos 1 1) (Pos 1 1)) "code generation is not implemented")

-- | The syntax tree of Racc source, one declaration per line.
parseTree :: ByteString -> Either Diagnostic String
parseTree src = unlines . map sexpr <$> syntax src

syntax :: ByteString -> Either Diagnostic [Located Decl]
syntax src = parse =<< tokenize src
