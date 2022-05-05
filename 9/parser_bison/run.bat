@echo off
flex lexer.l
bison -dy parser.y
gcc -o calc *.c
erase lex.yy.c y.tab.?
calc.exe < input.txt
