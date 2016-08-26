module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

-- Geração do analisador léxico
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops   = ["+", "*", "<=", "==", "=", ";", ":"]
    names = ["int", "bool", "true", "false", "noop", "if", "then", "else",
             "while", "do", "begin", "decl", "end"]
    style = emptyDef {
                Tok.commentLine     = "//"
              , Tok.commentStart    = "/*"
              , Tok.commentEnd      = "*/"
              , Tok.reservedOpNames = ops
              , Tok.reservedNames   = names
            }

integer :: Parser Integer
integer = Tok.integer lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
