module Language.TaPL.TypedArith (eval, eval', parseString, parseFile, Term(..), typeOf) where

import Language.TaPL.TypedArith.Syntax (Term(..))
import Language.TaPL.TypedArith.Parser (parseString, parseFile)
import Language.TaPL.TypedArith.Eval (eval, eval')
import Language.TaPL.TypedArith.Types (typeOf)

-- implements Fig. 8-2