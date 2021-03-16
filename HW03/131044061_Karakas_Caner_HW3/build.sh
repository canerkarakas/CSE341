#!/bin/sh
yacc -y -d gpp_interpreter.y
flex gpp_lexer.l
gcc lex.yy.c y.tab.c -o gpp -g -lm
