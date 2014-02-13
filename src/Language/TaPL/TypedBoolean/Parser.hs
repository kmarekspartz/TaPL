module Language.TaPL.TypedBoolean.Parser (parseString, parseFile) where

import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>))

import Language.TaPL.TypedBoolean.Syntax (Term(..))


-- This module is adapted from: http://www.haskell.org/haskellwiki/Parsing_a_simple_imperative_language

booleanDef =
  emptyDef { Token.reservedNames = [ "if"
                                   , "then"
                                   , "else"
                                   , "true"
                                   , "false"
                                   ]
           }


lexer = Token.makeTokenParser booleanDef
reserved   = Token.reserved   lexer -- parses a reserved name
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
whiteSpace = Token.whiteSpace lexer -- parses whitespace


booleanParser :: Parser Term
booleanParser = whiteSpace >> expr


expr :: Parser Term
expr =   parens expr
     <|> ifExpr
     <|> (reserved "true" >> return TmTrue)
     <|> (reserved "false" >> return TmFalse)

ifExpr :: Parser Term
ifExpr = do reserved "if"
            t1 <- expr
            reserved "then"
            t2 <- expr
            reserved "else"
            t3 <- expr
            return $ TmIf t1 t2 t3


parseString :: String -> Term
parseString str =
  case parse booleanParser "" str of
    Left e  -> error $ show e
    Right t -> t


parseFile :: String -> IO Term
parseFile file =
  do program  <- readFile file
     case parse booleanParser "" program of
       Left e  -> print e >> fail "parse error"
       Right t -> return t
