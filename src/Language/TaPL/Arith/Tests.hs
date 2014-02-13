module Language.TaPL.Arith.Tests where

import Test.QuickCheck
import Text.Printf

import Language.TaPL.ShowPretty (showp)
import Language.TaPL.Arith.Syntax (Term)
import Language.TaPL.Arith.Parser (parseString)
import Language.TaPL.Arith.Eval (eval, eval')


main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests


-- Both eval functions yield the same result.
prop_evaluates_the_same :: Term -> Bool
prop_evaluates_the_same t = eval t == eval' t


-- parse . showp is an identity function.
prop_showp_parse_id :: Term -> Bool
prop_showp_parse_id t = t == parseString (showp t)


-- eval parse . showp evaluates the same as eval.
prop_showp_parse_evaluates_the_same :: Term -> Bool
prop_showp_parse_evaluates_the_same t = eval (parseString (showp t)) == eval t


-- eval' parse . showp evaluates the same as eval'.
prop_showp_parse_evaluates_the_same' :: Term -> Bool
prop_showp_parse_evaluates_the_same' t = eval' (parseString (showp t)) == eval' t


tests  = [("evaluates_the_same", quickCheck prop_evaluates_the_same)
         ,("showp_parse_id", quickCheck prop_showp_parse_id)
         ,("showp_parse_evaluates_the_same", quickCheck prop_showp_parse_evaluates_the_same)
         ,("showp_parse_evaluates_the_same'", quickCheck prop_showp_parse_evaluates_the_same')
         ]
