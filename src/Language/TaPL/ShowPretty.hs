-- | Provides a type class for pretty printing.
module Language.TaPL.ShowPretty (ShowPretty, showp) where


-- | Pretty printing type class.
class ShowPretty a where
    -- | Converts an instance of ShowPretty into a pretty string.
    showp :: a -> String
