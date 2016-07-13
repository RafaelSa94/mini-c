#pragma once

#include "ast.hpp"
#include "lexer.hpp"
#include <vector>
#include <iostream>
#include <memory>

namespace helper {
// Cloning make_unique here until it's standard in C++14.
// Using a namespace to avoid conflicting with MSVC's std::make_unique (which
// ADL can sometimes find in unqualified calls).
template <class T, class... Args>
static
    typename std::enable_if<!std::is_array<T>::value, std::unique_ptr<T>>::type
    make_unique(Args &&... args) {
      return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
    }
} // end namespace helper

using namespace std;

class Parser {
private:
  vector<string> words;
  int currentLine;
  int opPrecedence(Token t);
  Token currentToken;
  Lexer* lexer;

  void LogError(const char *Str);

  unique_ptr<ExprAST> LogErrorExpr(const char *Str);
  unique_ptr<ExprAST> Expr();
  unique_ptr<ExprAST> BinOpRHSExpr(int ExprPrec, unique_ptr<ExprAST> LHS);
  unique_ptr<ExprAST> PrimaryExpr();
  unique_ptr<ExprAST> BinaryExpr();
  unique_ptr<ExprAST> BooleanExpr();
  unique_ptr<ExprAST> IntegerExpr();
  unique_ptr<ExprAST> VariableExpr();

  unique_ptr<DeclarationAST> LogErrorDecl(const char *Str);
  unique_ptr<DeclarationAST> Decl();
  unique_ptr<DeclarationAST> SimpleDecl();

  unique_ptr<CommandAST> LogErrorCommand(const char *Str);
  unique_ptr<CommandAST> Command();
  unique_ptr<CommandAST> BinCommand();
  unique_ptr<CommandAST> PrimaryCommand();
  unique_ptr<CommandAST> NoopCommand();
  unique_ptr<CommandAST> IfCommand();
  unique_ptr<CommandAST> WhileCommand();
  unique_ptr<CommandAST> AssignCommand();

  unique_ptr<ProgramAST> LogErrorProgram(const char *Str);
  unique_ptr<ProgramAST> Program();

public:
  void nextToken();

  Parser (vector<string> words);
  ~Parser();
};
