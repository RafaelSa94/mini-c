module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok


import Lexer
import Ast

binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

table = [[binary "*" Times Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft]
        ,[binary "<=" LessEqual Ex.AssocLeft,
          binary "==" Equal Ex.AssocLeft]]

intType :: Parser Type
intType = do
  reserved "int"
  return Int

boolType :: Parser Type
boolType = do
  reserved "bool"
  return Bool

parseType :: Parser Type
parseType = try intType
         <|> try boolType

declS :: Parser Decl
declS = do
  var <- identifier
  reservedOp ":"
  t  <- parseType
  return $ DeclS var t

declD :: Parser Decl
declD = do
  decl1 <- declS
  reservedOp ";"
  decl2 <- parseDecl
  return $ DeclD decl1 decl2

parseDecl :: Parser Decl
parseDecl = try declD
         <|> try declS

int :: Parser Expr
int = do
  n <- integer
  return $ Integer n

boolTrue :: Parser Expr
boolTrue = do
  reserved "true"
  return Ast.True

boolFalse :: Parser Expr
boolFalse = do
  reserved "false"
  return Ast.False

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor = try int
      <|> try boolFalse
      <|> try boolTrue
      <|> try variable

noopCommand :: Parser Command
noopCommand = do
  reserved "noop"
  return Noop

attCommand :: Parser Command
attCommand = do
  var <- identifier
  reservedOp "="
  ex  <- expr
  return $ Attribution var ex

commandD :: Parser Command
commandD = do
  c1 <- parseSimpleCommand
  reservedOp ";"
  c2 <- parseCommand
  return $ CommandD c1 c2

ifCommand :: Parser Command
ifCommand = do
  reserved "if"
  e <- expr
  reserved "then"
  c1 <- parseCommand
  reserved "else"
  c2 <- parseCommand
  return $ IfElse e c1 c2

whileCommand :: Parser Command
whileCommand = do
  reserved "while"
  e <- expr
  reserved "do"
  command <- parseCommand
  return $ WhileDo e command

parseSimpleCommand :: Parser Command
parseSimpleCommand = try noopCommand
                  <|> try attCommand
                  <|> try ifCommand
                  <|> try whileCommand

parseCommand :: Parser Command
parseCommand = try commandD
            <|> try parseSimpleCommand

parseProgram :: Parser Program
parseProgram = do
  reserved "decl"
  decl <- parseDecl
  reserved "begin"
  command <- parseCommand
  reserved "end"
  return $ Program decl command

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseTopLevel :: String -> Either ParseError Program
parseTopLevel = parse (contents parseProgram) "<error>"
