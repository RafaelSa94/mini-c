#include "../header/parser.hpp"

Parser::Parser(vector<string> words){
        this->words = words;
        this->lexer = new Lexer();
        this->currentLine = 0;
        this->nextToken();
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
                return LogErrorExpr("Error: Expecting expression\n");
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
                auto RHS = PrimaryExpr();
                if (!RHS)
                        return nullptr;

                int NextPrec = this->opPrecedence(this->lexer->getToken());

                if (TokPrec < NextPrec) {
                        RHS = BinOpRHSExpr(TokPrec + 1, move(RHS));
                        if(!RHS)
                                return nullptr;
                }

                LHS = helper::make_unique<BinaryExprAST> (binOp, move(LHS), move(RHS));
        }
}

unique_ptr<DeclarationAST> Parser::LogErrorDecl(const char *Str) {
        LogError(Str);
        return nullptr;
}

unique_ptr<DeclarationAST> Parser::SimpleDecl() {
        std::string identifier = this->lexer->IdentifierStr;
        this->nextToken();

        if(this->lexer->getToken() != Token::DeclOp)
                return LogErrorDecl("Error while parsing declaration ':' missing \n");

        this->nextToken();
        Type t;

        switch (this->lexer->getToken()) {
        default:
                return LogErrorDecl("Error while parsing declaration, wrong type \n");
        case Token::Integer:
                t = Type::Int;
                break;
        case Token::Bool:
                t = Type::Bool;
                break;
        }
        auto Result = helper::make_unique<SimpleDeclarationAST>(identifier, t);
        this->nextToken();
        return move(Result);
}

unique_ptr<DeclarationAST> Parser::Decl() {
        auto LHS = SimpleDecl();

        if(this->lexer->getToken() == Token::EndCommand) {
                this->nextToken();
                auto RHS = this->Decl();
                return helper::make_unique<CompositionDeclarationAST>(move(LHS), move(RHS));
        }

        if(this->lexer->getToken() != Token::Begin)
                return LogErrorDecl("Error, expecting 'begin' \n");
        return move(LHS);
}

unique_ptr<CommandAST> Parser::LogErrorCommand(const char *Str) {
        LogError(Str);
        return nullptr;
}

unique_ptr<CommandAST> Parser::AssignCommand() {
  if (this->lexer->getToken() != Token::Identifier) {
    return LogErrorCommand("Error, Expecting identifier\n");
  }

  string id = this->lexer->IdentifierStr;
  this->nextToken();

  if (this->lexer->getToken() != Token::AssignOp) {
    return LogErrorCommand("Error, Expecting '='\n");
  }

  this->nextToken();
  auto expr = this->Expr();

  auto assign = helper::make_unique<AssignCommandAST>(id, move(expr));
  this->nextToken();
  return move(assign);
}

unique_ptr<CommandAST> Parser::WhileCommand() {
  if (this->lexer->getToken() != Token::While)
    return LogErrorCommand("Error, expecting 'while'\n");

  this->nextToken();
  auto expr = this->Expr();
  this->nextToken();

  if (this->lexer->getToken() != Token::Do)
    return LogErrorCommand("Error, expecting 'do'\n");

  this->nextToken();
  auto c = this->Command();
  auto R = helper::make_unique<WhileCommandAST>(move(expr),move(c));
  this->nextToken();
  return move(R);
}

unique_ptr<CommandAST> Parser::IfCommand() {
  if(this->lexer->getToken() != Token::If){
    return LogErrorCommand("Error, expecting 'if'\n");
  }

  this->nextToken();
  auto e = this->Expr();
  this->nextToken();

  if(this->lexer->getToken() != Token::Then){
    return LogErrorCommand("Error, expecting 'then'\n");
  }

  this->nextToken();
  auto c1 = this->Command();
  this->nextToken();

  if(this->lexer->getToken() != Token::Else){
    return LogErrorCommand("Error, expecting 'else'\n");
  }

  this->nextToken();
  auto c2 = this->Command();
  auto if_then_else = helper::make_unique<IfCommandAST>(move(e),move(c1),move(c2));
  this->nextToken();
  return move(if_then_else);
}

unique_ptr<CommandAST> Parser::NoopCommand() {
  if(this->lexer->getToken() != Token::Noop){
    return LogErrorCommand("Error, expecting 'noop'\n");
  }

  auto r = helper::make_unique<NoopCommandAST>();
  this->nextToken();
  return move(r);
}

unique_ptr<CommandAST> Parser::PrimaryCommand() {
  switch (this->lexer->getToken()) {
    default:
      return AssignCommand();
    case Token::Noop:
      return NoopCommand();
    case Token::If:
      return IfCommand();
    case Token::While:
      return WhileCommand();
  }

}

unique_ptr<CommandAST> Parser::Command(){
  auto LHS = this->PrimaryCommand();

  if(this->lexer->getToken() == Token::EndCommand) {
    this->nextToken();

    auto RHS = this->Command();

    return helper::make_unique<CompositionCommandAST>(move(LHS), move(RHS));
  }

  if(this->lexer->getToken() != Token::End) {
    return LogErrorCommand("Error, expecting 'end' \n");
  }
  this->nextToken();
  return move(LHS);
}

unique_ptr<ProgramAST> Parser::LogErrorProgram(const char *Str){
  LogError(Str);
  return nullptr;
}

unique_ptr<ProgramAST> Parser::Program() {
  if(this->lexer->getToken() != Token::Decl){
    return LogErrorProgram("Error, expecting 'decl'");
  }

  this->nextToken();
  auto decl = this->Decl();

  if(this->lexer->getToken() != Token::Begin) {
    return LogErrorProgram("Error, expecting 'begin'");
  }

  this->nextToken();
  auto c = this->Command();

  if(this->lexer->getToken() != Token::End) {
    return LogErrorProgram("Error, expecting 'end'");
  }

  auto p = helper::make_unique<ProgramAST>(move(decl),move(c));
  return p;
}
