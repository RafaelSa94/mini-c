CC=clang++
flags= -std=c++11 `llvm-config --cxxflags`
libs=src/lexer.cpp

main:
	$(CC) $(flags) -o main main.cpp $(libs)
