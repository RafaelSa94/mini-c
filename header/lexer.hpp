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
  End
};
Token bunda = Token::Begin;

struct Lexer {
private:
  std::string IdentifierStr;
  int NumVal;
  Token last;
public:
  void getToken(std::string word);
  Token() { }
  virtual ~Token() { }
};