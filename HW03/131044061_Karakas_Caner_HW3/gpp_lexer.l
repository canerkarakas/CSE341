%option noyywrap

%{
	#include <string.h>
	#include <stdio.h>
    #include <stdlib.h>
    #include <stdarg.h>
    #include <math.h>
    #include "y.tab.h"
%}



digit [0-9]
VALUE -?{digit}+(\.{digit}+)?
ID [a-zA-Z][_a-zA-Z0-9]*


%%

{VALUE}			{   yylval.integerVal = strtod(yytext, NULL);
				return VALUE; }
";;".*			{ return COMMENT; }
"and"       	{ return KW_AND; }
"or"            { return KW_OR; }
"not"           { return KW_NOT; }
"equal"         { return KW_EQUAL; }
"less"          { return KW_LESS; }
"nil"           { return KW_nil; }
"list"          { return KW_LIST; }
"append"        { return KW_APPEND; }
"concat"        { return KW_CONCAT; }
"set"           { return KW_SET; }
"deffun"        { return KW_DEFFUN; }
"for"           { return KW_FOR;}
"if"            { return KW_IF; }
"exit"          { return KW_EXIT; }
"load"          { return KW_LOAD; }
"disp"          { return KW_DISP; }
"true"          { yylval.str = (char *) malloc(strlen(yytext)*sizeof(char));
                  strcpy(yylval.str, yytext);
                  return KW_TRUE; }
"false"         { yylval.str = (char *) malloc(strlen(yytext)*sizeof(char));
                  strcpy(yylval.str, yytext);
                  return KW_FALSE; }
"("      		{ return OP_OP; }
")"      		{ return OP_CP; }
","      		{ return OP_COMMA; }
"+"	 			{ return OP_PLUS; }
"-"	 			{ return OP_MINUS; }
"/"	 			{ return OP_DIV; }
"*"	 			{ return OP_MULT; }
"**"	 		{ return OP_DBLMULT; }
"'"			    { return OP_OC; }
"\""            { return OP_STR;}
{ID}            { yylval.str = (char *) malloc(strlen(yytext)*sizeof(char));
                  return ID; }
[ \t\r\f]+	;/*skip whitespace*/
.		   // yerror("lex: error");
%%
 
