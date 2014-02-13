module Language.TaPL.Boolean.Syntax (Term(..), isVal) where

import Test.QuickCheck (Arbitrary, arbitrary, sized, oneof)
import Control.Monad (liftM3)
import Control.Applicative ((<$>))


data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
  deriving (Eq, Read)


isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal _ = False
  

instance Show Term where
  show TmTrue = "true"
  show TmFalse = "false"
  show (TmIf t1 t2 t3) = "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3


instance Arbitrary Term where
  arbitrary = arbitraryTerm

arbitraryTerm = sized arbitraryTerm'
arbitraryTerm' 0 = oneof $ return <$> [ TmTrue
                                      , TmFalse
                                      ]
arbitraryTerm' n | n > 0 = oneof [ return TmTrue
                                 , return TmFalse
                                 , liftM3 TmIf subterm subterm subterm
                                 ]
  where subterm = arbitraryTerm' (n `div` 2)
