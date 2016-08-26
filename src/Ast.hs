module Ast where

type Name = String

-- Tipo da varíavel Inteiro ou Booleano
data Type = Int
          | Bool
          deriving (Eq, Ord, Show)

{- Declarações de variável podem ser:
   Decl -> x : Tipo | Decl; Decl
-}
data Decl = DeclD Decl Decl
          | DeclS Name Type
          deriving (Eq, Ord, Show)

{- Expressões podem ser:
   Expr -> x | Booleano | Número | x `OP` x | Expr; Expr
-}

data Expr = Var Name
          | True
          | False
          | Integer Integer
          | BinOp Op Expr Expr
          deriving (Eq, Ord, Show)

-- Operações de soma, multiplicação e comparação
data Op = Plus
        | Times
        | LessEqual
        | Equal
        deriving (Eq, Ord, Show)

data Command = Noop
             | Attribution Name Expr
             | CommandD Command Command
             | IfElse Expr Command Command
             | WhileDo Expr Command
             deriving (Eq, Ord, Show)

data Program = Program Decl Command deriving (Eq, Ord, Show)
