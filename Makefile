all:
	mkdir -p bin
	./bootstrap ./src/Compiler.c07 linux64exe -out ./bin/c07 -stk 2
