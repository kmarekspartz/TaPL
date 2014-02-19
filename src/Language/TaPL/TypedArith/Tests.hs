-- | Tests some properties against Language.TaPL.TypedArith.
module Language.TaPL.TypedArith.Tests where

import Control.Applicative ((<$>))
import Test.QuickCheck (quickCheck)
import Text.Printf (printf)

import Language.TaPL.ShowPretty (showp)
import Language.TaPL.TypedArith (Term, parseString, eval, eval', typeOf)


-- | A test runner.
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests


-- | Both eval functions yield the same result.
prop_evaluates_the_same :: Term -> Bool
prop_evaluates_the_same t = eval t == eval' t


-- | parse . showp is an identity function.
prop_showp_parse_id :: Term -> Bool
prop_showp_parse_id t = t == parseString (showp t)


-- | eval . parse . showp evaluates the same as eval.
prop_showp_parse_evaluates_the_same :: Term -> Bool
prop_showp_parse_evaluates_the_same t = eval (parseString (showp t)) == eval t


-- | eval' . parse . showp evaluates the same as eval'.
prop_showp_parse_evaluates_the_same' :: Term -> Bool
prop_showp_parse_evaluates_the_same' t = eval' (parseString (showp t)) == eval' t


-- | typeOf before eval is the same after eval.
prop_typeOf_eval :: Term -> Bool
prop_typeOf_eval t = 
    case eval t of
        Just t' -> typeOf t == typeOf t'
        Nothing -> True


-- | typeOf before eval' is the same after eval'.
prop_typeOf_eval' :: Term -> Bool
prop_typeOf_eval' t = 
    case eval' t of
        Just t' -> typeOf t == typeOf t'
        Nothing -> True
        
-- | The resulting type of evaluating should be the same for both evaluators.
prop_typeOf_eval_the_same :: Term -> Bool
prop_typeOf_eval_the_same t = (typeOf <$> eval' t) == (typeOf <$> eval t)


-- | List of tests and their names.
tests  = [("evaluates_the_same", quickCheck prop_evaluates_the_same)
         ,("showp_parse_id", quickCheck prop_showp_parse_id)
         ,("showp_parse_evaluates_the_same", quickCheck prop_showp_parse_evaluates_the_same)
         ,("showp_parse_evaluates_the_same'", quickCheck prop_showp_parse_evaluates_the_same')
         ,("prop_typeOf_eval", quickCheck prop_typeOf_eval)
         ,("prop_typeOf_eval'", quickCheck prop_typeOf_eval')
         ,("prop_typeOf_eval_the_same", quickCheck prop_typeOf_eval_the_same)
         ]
