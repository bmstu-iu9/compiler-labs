@echo off
flex lexer.l
bison -dy parser.y
gcc -o calc *.c
erase lex.yy.c y.tab.?
echo Run calc.exe for test