module Language.TaPL.Boolean.Eval (eval, eval') where

import Control.Monad (liftM)
import Language.TaPL.Boolean.Syntax (Term(..))


-- Small Step
 
eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) = liftM (\t1' -> TmIf t1' t2 t3) $ eval1 t1

eval1 _ = Nothing


eval :: Term -> Maybe Term
eval t = case eval1 t of
           Nothing -> Just t
           Just t' -> eval t'


-- Big Step

eval' :: Term -> Maybe Term
eval' (TmIf t1 t2 t3) =
  case eval' t1 of
    Just TmTrue -> eval' t2
    Just TmFalse -> eval' t3
    Just t -> Just $ TmIf t t2 t3

eval' t = Just t
