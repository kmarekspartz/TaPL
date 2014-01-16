module Language.TaPL.Arith.Eval (eval, eval') where

import Control.Monad (liftM)

import Language.TaPL.Arith.Syntax (Term(..))


-- Small Step
 
eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) = liftM (\t1' -> TmIf t1' t2 t3) $ eval1 t1

eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc t)) = Just t
eval1 (TmPred t) = liftM TmPred $ eval1 t

eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc _)) = Just TmFalse
eval1 (TmIsZero t) = liftM TmIsZero $ eval1 t

eval1 (TmSucc t) = liftM TmSucc $ eval1 t

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

eval' (TmPred t) =
  case eval' t of
    Just TmZero -> Just TmZero
    Just (TmSucc t') -> Just t'
    Just t' -> Just $ TmPred t'

eval' (TmSucc t) = liftM TmSucc $ eval' t
     
eval' (TmIsZero t) =
  case eval' t of
    Just TmZero -> Just TmTrue
    Just (TmSucc t') -> Just TmFalse
    Just t' -> Just $ TmIsZero t'

eval' t = Just t

    



