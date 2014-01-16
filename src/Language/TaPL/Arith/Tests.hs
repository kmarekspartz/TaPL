module Language.TaPL.Arith.Tests where

import Test.QuickCheck
import Text.Printf

import Language.TaPL.Arith.Syntax (Term)
import Language.TaPL.Arith.Parser (parseString)
import Language.TaPL.Arith.Eval (eval, eval')


main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests


-- Both eval functions yield the same result.
prop_evaluates_the_same :: Term -> Bool
prop_evaluates_the_same t = eval t == eval' t


-- parse . show is an identity function.
prop_show_parse_id :: Term -> Bool
prop_show_parse_id t = t == parseString (show t)


-- eval parse . show evaluates the same as eval.
prop_show_parse_evaluates_the_same :: Term -> Bool
prop_show_parse_evaluates_the_same t = eval (parseString (show t)) == eval t


-- eval' parse . show evaluates the same as eval'.
prop_show_parse_evaluates_the_same' :: Term -> Bool
prop_show_parse_evaluates_the_same' t = eval' (parseString (show t)) == eval' t


tests  = [("evaluates_the_same", quickCheck prop_evaluates_the_same)
         ,("show_parse_id", quickCheck prop_show_parse_id)
         ,("show_parse_evaluates_the_same", quickCheck prop_show_parse_evaluates_the_same)
         ,("show_parse_evaluates_the_same'", quickCheck prop_show_parse_evaluates_the_same')
         ]
