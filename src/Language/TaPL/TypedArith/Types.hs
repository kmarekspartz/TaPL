module Language.TaPL.TypedArith.Types (typeOf, Ty) where
    
import Language.TaPL.TypedArith.Syntax (Term(..))
    

data Ty = TyBool
        | TyNat
    deriving (Show, Read, Eq)


typeOf :: Term -> Maybe Ty
typeOf TmZero = Just TyNat

typeOf (TmSucc t) | typeOf t == Just TyNat = Just TyNat
typeOf (TmPred t) | typeOf t == Just TyNat = Just TyNat

typeOf (TmIsZero t) | typeOf t == Just TyNat = Just TyBool

typeOf TmTrue = Just TyBool
typeOf TmFalse = Just TyBool

typeOf (TmIf t1 t2 t3) | typeOf t1 == Just TyBool && typeOf t2 == typeOf t3 = typeOf t2

typeOf _ = Nothing
