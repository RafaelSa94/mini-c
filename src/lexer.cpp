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

  if (std::regex_match(word, Integer)) {
    this->NumVal = std::atoi(word.c_str());
    this->last   = Token::Integer;
  }
  else if(std::regex_match(word, Identifier)) {
    //if's aninhados... :(
    if (!word.compare("if")) {
      this->last = Token::If;
    }
    else if (!word.compare("then")) {
      this->last = Token::Then;
    }

    else if (!word.compare("else")) {
      this->last = Token::Else;

    }

    else if (!word.compare("false")) {
      this->last = Token::Bool;
    }

    else if (!word.compare("true")) {
      this->last = Token::Bool;
    }

    else if (!word.compare("while")) {
      this->last = Token::While;

    }

    else if (!word.compare("do")) {
      this->last = Token::Do;
    }

    else if (!word.compare("bool")) {
      this->last = Token::Type;

    }

    else if (!word.compare("int")) {
      this->last = Token::Type;
    }

    else if (!word.compare("decl")) {
      this->last = Token::Decl;
    }

    else if (!word.compare("end")) {
      this->last = Token::End;
    }

    else {
      this->last = Token::Identifier;
      this->IdentifierStr = word;
    }
  }
  else {
    if(!word.compare("+")) {
      this->last = Token::PlusOp;
    }
    else if (!word.compare("*")) {
      this->last = Token::TimesOp;
    }
    else if (!word.compare("=")) {
      this->last = Token::AssignOp;
    }
    else if (!word.compare("<=")) {
      this->last = Token::LessEq;
    }
    else if (!word.compare("==")) {
      this->last = Token::Equal;
    }
    else if (!word.compare(":")) {
      this->last = Token::DeclOp;
    }
    else if (!word.compare(";")) {
      this->last = Token::EndCommand;
    }
    else {
      this->last = Token::ERROR;
    }
  }
}
