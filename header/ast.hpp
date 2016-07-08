#pragma once

#include <memory>
#include <string>

enum class Type {
  Int,
  Bool
};

class DeclarationAST {
private:
  std::string var;
  Type t;
public:
  DeclarationAST (std::string &var, Type t) : var(var), t(t) {}
};

class SimpleDeclarationAST : public DeclarationAST{
private:
  std::string var;
  Type t;
public:
  SimpleDeclarationAST (std::string &var, Type t) : var(var), t(t) {}
};

class CompositionDeclarationAST : public DeclarationAST {
private:
  std::unique_ptr<DeclarationAST> d1;
  std::unique_ptr<DeclarationAST> d2;
public:
  CompositionDeclarationAST  (std::unique_ptr<DeclarationAST> d1, std::unique_ptr<DeclarationAST> d2)
    : d1(std::move(d1)), d2(std::move(d2)) {}
};

class ExprAST {
public:
  virtual ~ExprAST() {}
};

class VariableExprAST : public ExprAST {
private:
  std::string var;
public:
  VariableExprAST (std::string &var) : var(var) {}
};

class BooleanExprAST : public ExprAST {
private:
  bool value;
public:
  BooleanExprAST(bool value) : value(value) {}
};

class IntegerExprAST : public ExprAST {
private:
  int value;
public:
  IntegerExprAST(int value) : value(value) {}
};

enum class ExprOp {
  Plus,
  Times,
  LessEq,
  Equal
};

class BinaryExprAST : public ExprAST {
private:
  ExprOp Op;
  std::unique_ptr<ExprAST> Left, Right;
public:
  BinaryExprAST(ExprOp Op, std::unique_ptr<ExprAST> Left,
                std::unique_ptr<ExprAST> Right)
    : Op(Op), Left(std::move(Left)), Right(std::move(Right)) {}
};

class CommandAST {
public:
  virtual ~CommandAST() {}
};

class NoopCommandAST : public CommandAST {
public:
  NoopCommandAST() {}
};

class AssignCommandAST : public CommandAST{
private:
  std::string var;
  std::unique_ptr<ExprAST> expr;
public:
  AssignCommandAST(std::string &var, std::unique_ptr<ExprAST> expr)
    : var(var), expr(std::move(expr)) {}
};

class CompositionCommandAST : public CommandAST {
private:
  std::unique_ptr<CommandAST> c1;
  std::unique_ptr<CommandAST> c2;
public:
  CompositionCommandAST(std::unique_ptr<CommandAST> c1,
                        std::unique_ptr<CommandAST> c2)
    : c1(std::move(c1)), c2(std::move(c2)) {}
};

class IfCommandAST : public CommandAST {
private:
  std::unique_ptr<ExprAST> condition;
  std::unique_ptr<CommandAST> then_command;
  std::unique_ptr<CommandAST> else_command;
public:
  IfCommandAST(std::unique_ptr<ExprAST> condition,
               std::unique_ptr<CommandAST> then_command,
               std::unique_ptr<CommandAST> else_command)
   : condition(std::move(condition)), then_command(std::move(else_command)), else_command(else_command) {}
};

class WhileCommandAST : public CommandAST {
private:
  std::unique_ptr<ExprAST> condition;
  std::unique_ptr<CommandAST> command;
public:
  WhileCommandAST (std::unique_ptr<ExprAST> condition, std::unique_ptr<CommandAST> command)
   : condition(std::move(condition)), command(std::move(command)) {}
};

class ProgramAST {
private:
  std::unique_ptr<DeclarationAST> decl;
  std::unique_ptr<CommandAST> commands;
public:
  ProgramAST(std::unique_ptr<DeclarationAST> decl, std::unique_ptr<CommandAST> commands)
   : decl(std::move(decl)), commands(std::move(commands)) {}
};
