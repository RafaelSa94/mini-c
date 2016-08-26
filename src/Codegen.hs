module Codegen where

import Ast

declaration :: Decl -> String
declaration (DeclD d1 d2) = (declaration d1) ++ (declaration d2)
declaration (DeclS name _) = "int " ++ name ++ ";\n"

expression :: Expr -> String
expression (Var v) = " v "
expression Ast.True = " 1 "
expression Ast.False = " 0 "
expression (Integer n) = show n
expression (BinOp op e1 e2) =
  case op of
    LessEqual -> (expression e1) ++ " <= " ++ (expression e2)
    Equal     -> (expression e1) ++ " == " ++ (expression e2)
    Times     -> (expression e1) ++ " * "  ++ (expression e2)
    Plus      -> (expression e1) ++ " + "  ++ (expression e2)

command :: Command -> String
command Noop = "\n"
command (Attribution var expr) = var ++ "=" ++ (expression expr) ++ ";\n"
command (WhileDo expr c) = "while(" ++ (expression expr) ++ "){\n"
  ++ (command c) ++ "}\n"
command (IfElse expr c1 c2) = "if(" ++ (expression expr) ++ "){"
    ++ (command c1) ++ "}else{\n" ++ (command c2) ++ "}\n"

program :: Program -> String
program (Program decls cmds) = "int main(){\n" ++ (declaration decls) ++(command cmds)
  ++ "return 0;}"
