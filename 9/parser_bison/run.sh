#!/bin/bash

lex lexer.l
yacc -d parser.y
gcc -o calc *.c
rm -f lex.yy.c y.tab.?
./calc < input.txt
