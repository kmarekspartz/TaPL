-- | Implements Figure 8-1.
module Language.TaPL.TypedBoolean (eval, eval', parseString, parseFile, Term(..), typeOf) where

import Language.TaPL.TypedBoolean.Syntax (Term(..))
import Language.TaPL.TypedBoolean.Parser (parseString, parseFile)
import Language.TaPL.TypedBoolean.Eval (eval, eval')
import Language.TaPL.TypedBoolean.Types (typeOf)
