module Language.TaPL.Boolean.Eval (eval, eval') where

import Control.Applicative ((<$>))

import Language.TaPL.Boolean.Syntax (Term(..), isVal)


-- Small Step
 
eval1 :: Term -> Maybe Term
eval1 t | isVal t   = Just t

eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) | not $ isVal t1 = (\t1' -> TmIf t1' t2 t3) <$> eval1 t1

eval1 _ = Nothing


eval :: Term -> Maybe Term
eval t | isVal t = Just t
       | otherwise =
           case eval1 t of
             Nothing -> Nothing
             Just t' -> eval t'


-- Big Step

eval' :: Term -> Maybe Term
eval' (TmIf t1 t2 t3) =
  case eval' t1 of
    Just TmTrue -> eval' t2
    Just TmFalse -> eval' t3
    _ -> Nothing

eval' t | isVal t = Just t
        | otherwise = Nothing
