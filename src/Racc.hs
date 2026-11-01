module Racc (compile) where

import Data.ByteString (ByteString)
import Racc.Diagnostic (Diagnostic (Diagnostic), Pos (Pos), Span (Span))
import Racc.Lexer (tokenize)
import Racc.Parser (parse)

-- | Compile Racc source to C.
compile :: ByteString -> Either Diagnostic String
compile src = do
  _ <- parse =<< tokenize src
  Left (Diagnostic (Span (Pos 1 1) (Pos 1 1)) "code generation is not implemented")
