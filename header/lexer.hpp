#pragma once

#include <iostream>
#include <regex>

enum class Token{
  Type,
  Identifier,
  DeclOp,
  EndCommand,
  Integer,
  Bool,
  PlusOp,
  TimesOp,
  AssignOp,
  LessEq,
  Equal,
  If,
  Then,
  Else,
  While,
  Do,
  Decl,
  Begin,
  End,
  ERROR
};



struct Lexer {
private:
  Token last;
public:
  void nextToken(std::string word);
  Token getToken() {return this->last;}

  std::string IdentifierStr;
  int NumVal;
  bool BoolVal;

  Lexer() { }
  virtual ~Lexer() { }
};
