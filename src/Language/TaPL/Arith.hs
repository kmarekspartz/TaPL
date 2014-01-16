module Language.TaPL.Arith (eval, eval', parseString, parseFile, Term(..)) where

import Language.TaPL.Arith.Syntax (Term(..))
import Language.TaPL.Arith.Parser (parseString, parseFile)
import Language.TaPL.Arith.Eval (eval, eval')