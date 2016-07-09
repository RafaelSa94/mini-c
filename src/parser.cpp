#include "../header/parser.hpp"

Parser::Parser(vector<string> words){
  this->words = words;
  this->lexer = new Lexer();
  this->currentLine = 0;
}

Parser::~Parser(){
  free(this->lexer);
}

void Parser::nextToken(){
  this->lexer->nextToken(this->words[this->currentLine]);
  this->currentLine += 1;
}

int Parser::opPrecedence(Token t){
  switch(t) {
    default:
      return -1;
    case Token::Equal:
      return 10;
    case Token::LessEq:
      return 20;
    case Token::PlusOp:
      return 30;
    case Token::TimesOp:
      return 40;
  }
}

void Parser::LogError(const char *Str){
  fprintf(stderr, "LogError: %s at line %d\n", Str, this->currentLine);
}

unique_ptr<ExprAST> Parser::LogErrorExpr(const char *Str){
  LogError(Str);
  return nullptr;
}

unique_ptr<ExprAST> Parser::IntegerExpr(){
  auto Result = helper::make_unique<IntegerExprAST>(this->lexer->NumVal);
  this->nextToken();
  return move(Result);
}

unique_ptr<ExprAST> Parser::VariableExpr(){
  auto Result = helper::make_unique<VariableExprAST>(this->lexer->IdentifierStr);
  this->nextToken();
  return move(Result);
}

unique_ptr<ExprAST> Parser::BooleanExpr() {
  auto Result = helper::make_unique<BooleanExprAST>(this->lexer->BoolVal);
  this->nextToken();
  return move(Result);
}

unique_ptr<ExprAST> Parser::PrimaryExpr() {
  switch (this->lexer->getToken()) {
    default:
      return LogErrorExpr("Error: Expecting expression");
    case Token::Identifier:
      return VariableExpr();
    case Token::Integer:
      return IntegerExpr();
    case Token::Bool:
      return BooleanExpr();
  }
}

unique_ptr<ExprAST> Parser::Expr() {
  auto LHS = PrimaryExpr();
  if(!LHS)
    return nullptr;

  return BinOpRHSExpr(0, std::move(LHS));
}

unique_ptr<ExprAST> Parser::BinOpRHSExpr(int ExprPrec, unique_ptr<ExprAST> LHS) {
  while(1) {
    int TokPrec = this->opPrecedence(this->lexer->getToken());
    if(TokPrec < ExprPrec)
      return LHS;

    ExprOp binOp;
    switch(this->lexer->getToken()) {
      case Token::Equal:
        binOp = ExprOp::Equal;

      case Token::LessEq:
        binOp = ExprOp::LessEq;

      case Token::PlusOp:
        binOp = ExprOp::Plus;

      case Token::TimesOp:
        binOp = ExprOp::Times;
      }

    this->nextToken();
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

    int NextPrec = this->opPrecedence(this->lexer->getToken());

    if (TokPrec < NextPrec) {
      RHS = BinOpRHSExpr(TokPrec + 1, move(RHS));
      if(!RHS)
        return nullptr;
    }

    LHS = helper::make_unique<BinaryExprAST> (binOP, move(LHS), move(RHS));
  }
}
