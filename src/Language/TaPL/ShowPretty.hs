module Language.TaPL.ShowPretty (ShowPretty, showp) where

class ShowPretty a where
    showp :: a -> String
