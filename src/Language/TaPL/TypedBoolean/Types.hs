module Language.TaPL.TypedBoolean.Types (typeOf, Ty) where
    
import Language.TaPL.TypedBoolean.Syntax (Term(..))
    

data Ty = TyBool
    deriving (Show, Read, Eq)


typeOf :: Term -> Maybe Ty
typeOf TmTrue = Just TyBool
typeOf TmFalse = Just TyBool

typeOf (TmIf t1 t2 t3) | typeOf t1 == Just TyBool && typeOf t2 == typeOf t3 = typeOf t2

typeOf _ = Nothing
