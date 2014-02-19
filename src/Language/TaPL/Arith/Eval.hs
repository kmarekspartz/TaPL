-- | Evaluators for Language.TaPL.Arith.
module Language.TaPL.Arith.Eval (eval, eval') where

import Control.Applicative ((<$>))

import Language.TaPL.Arith.Syntax (Term(..), isVal, isNumericVal)


-- | Small step evaluator.
eval :: Term -> Maybe Term
eval t | isVal t = Just t
       | otherwise =
           case eval1 t of
             Nothing -> Nothing
             Just t' -> eval t'


-- | A single step.
eval1 :: Term -> Maybe Term
eval1 t | isVal t = Just t

eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) | not $ isVal t1 = (\t1' -> TmIf t1' t2 t3) <$> eval1 t1

eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc t)) | isNumericVal t = Just t
eval1 (TmPred t) | not $ isVal t = TmPred <$> eval1 t

eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc t)) | isNumericVal t = Just TmFalse
eval1 (TmIsZero t) | not (isVal t) = TmIsZero <$> eval1 t

eval1 (TmSucc t) | not $ isVal t = TmSucc <$> eval1 t

eval1 _ = Nothing


-- | Big step evaluator.
eval' :: Term -> Maybe Term
eval' (TmIf t1 t2 t3) =
  case eval' t1 of
    Just TmTrue -> eval' t2
    Just TmFalse -> eval' t3
    _ -> Nothing

eval' (TmPred t) =
  case eval' t of
    Just TmZero -> Just TmZero
    Just (TmSucc t') -> Just t'
    _ -> Nothing

eval' (TmSucc t) =
  case eval' t of
    Just TmZero -> Just $ TmSucc TmZero
    Just (TmSucc t) -> Just $ TmSucc $ TmSucc t
    _ -> Nothing
     
eval' (TmIsZero t) =
  case eval' t of
    Just TmZero -> Just TmTrue
    Just (TmSucc t') -> Just TmFalse
    _ -> Nothing

eval' t | isVal t = Just t
        | otherwise = Nothing
