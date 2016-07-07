#include "../header/lexer.hpp"


/*Função mágica pra identificar o token de uma palavra
  Caso seja número atribuir o this->numVal para ele
  se for identificador atribuir o this->IdentifierStr
  para ele.

  Ao final modificar o this->last para o token.
*/

void Lexer::getToken(std::string word) {
  static std::regex Integer("[:d:]+");
  static std::regex Identifier("[:alpha:][:w:]+"); // w = alphanum

  if (std::regex_match(Iteger, word)) {
    this->NumVal = std::atoi(word);
    this->last   = Token::Integer;
  }
  else if(std::regex_match(Identifier, word)) {
    //if's aninhados... :(
    else if (!word.compare(""))


  )
  else {
    if(!word.compare("+")) {

    }
    else if (!word.compare("*")) {

    }
    else if (!word.compare("=")) {

    }
    else if (!word.compare("<=")) {

    }
    else if (!word.compare("==")) {

    }


  }

  }
  static std::regex PlusOp("\\+");
  static std::regex TimesOp("\\*");
  static std::regex AssignOp("=");
  static std::regex LessEq("<=");
  static std::regex Equal("==");
  static std::regex If("if");
  static std::regex EndCommand(";");
  static std::regex DeclOp(":");
  static std::regex Then("then");
  static std::regex Else("else");
  static std::regex Bool("(false)|(true)");
  static std::regex While("while");
  static std::regex Do("do");
  static std::regex Type("(bool)|(int)");
  static std::regex Decl("decl");
  static std::regex Begin("begin");
  static std::regex End("end");


}
