module Language.TaPL.Arith.Syntax (Term(..), integerToTerm, isVal, isNumericVal) where

import Test.QuickCheck (Arbitrary, arbitrary, sized, oneof)
import Control.Monad (liftM, liftM3)
import Control.Applicative ((<$>))


data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
  deriving (Eq, Read)
  

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False


isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t


termToInteger :: Term -> Integer
termToInteger TmZero = 0
termToInteger (TmSucc nv) | isNumericVal nv = 1 + termToInteger nv
termToInteger _ = error "termToInteger called on non-numeric val"


integerToTerm :: Integer -> Term
integerToTerm n | n < 0  = error "integerToTerm called on a negative Integer"
                | n == 0 = TmZero
                | n > 0  = TmSucc $ integerToTerm (n - 1)


instance Show Term where
  show TmTrue = "true"
  show TmFalse = "false"
  show (TmIf t1 t2 t3) = "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3

  show nv | isNumericVal nv = show $ termToInteger nv
  show (TmPred t) = "pred " ++ show t
  show (TmIsZero t) = "iszero " ++ show t
  show (TmSucc t) = "succ " ++ show t


instance Arbitrary Term where
  arbitrary = arbitraryTerm

arbitraryTerm = sized arbitraryTerm'
arbitraryTerm' 0 = oneof $ return <$> [ TmTrue
                                      , TmFalse
                                      , TmZero
                                      ]
arbitraryTerm' n | n > 0 = oneof [ return TmTrue
                                 , return TmFalse
                                 , return TmZero
                                 , liftM TmSucc subterm
                                 , liftM TmPred subterm
                                 , liftM TmIsZero subterm
                                 , liftM3 TmIf subterm subterm subterm
                                 ]
  where subterm = arbitraryTerm' (n `div` 2)
