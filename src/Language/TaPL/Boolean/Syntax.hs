module Language.TaPL.Boolean.Syntax (Term(..), isVal) where

import Test.QuickCheck (Arbitrary, arbitrary, sized, oneof)
import Control.Monad (liftM3)
import Control.Applicative ((<$>))

import Language.TaPL.ShowPretty (ShowPretty, showp)


data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
  deriving (Eq, Read, Show)


isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal _ = False
  

instance ShowPretty Term where
  showp TmTrue = "true"
  showp TmFalse = "false"
  showp (TmIf t1 t2 t3) = "if " ++ showp t1 ++ " then " ++ showp t2 ++ " else " ++ showp t3


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
