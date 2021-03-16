%{

   #include <stdio.h>

   #include <stdlib.h>

   #include <string.h>

   void print(char *token);

   void error();

%}


alpha	   [a-z]

digit      [0-9]

alphanum   {alpha}|{digit}



ID         {alpha}+{alphanum}*

VALUE	   "0"|[0-9]{digit}*


%%


";;".*		{ printf("comment line"); } 


"and"           { print("KW_AND"); }

"or"            { print("KW_OR"); }

"not"           { print("KW_NOT"); }

"equal"         { print("KW_EQUAL"); }

"less"          { print("KW_LESS"); }

"nil"           { print("KW_nil"); }

"list"          { print("KW_LIST"); }

"append"        { print("KW_APPEND"); }

"concat"        { print("KW_CONCAT"); }

"set"           { print("KW_SET"); }

"deffun"        { print("KW_DEFFUN"); }

"for"           { print("KW_FOR"); }

"if"            { print("KW_IF"); }

"exit"          { print("KW_EXIT"); }

"load"          { print("KW_LOAD"); }

"disp"          { print("KW_DISP"); }

"true"          { print("KW_TRUE"); }

"false"         { print("KW_FALSE"); }


"("      { print("OP_OP"); }

")"      { print("OP_CP"); }

","      { print("OP_COMMA"); }

"+"	 { print("OP_PLUS"); }

"-"	 { print("OP_MINUS"); }

"/"	 { print("OP_DIV"); }

"*"	 { print("OP_MULT"); }

"**"	 { print("OP_DBLMULT"); }

""""	 { print("OP_OC OP_CC"); }

{ID}	 { print("ID"); }

{VALUE}  { print("VALUE"); }



[ \t\r\f]+


.		   { error("Unrecognized character"); }


%%


void print(char *token){

   printf("text: %s token: %s\n", yytext, token);

}


void error(char *output){

   printf("Error: \"%s\". Token = %s\n", output, yytext);

   exit(1);

}


int main(int argc, char *argv[]){

   yyin = fopen(argv[1], "r");

   yylex();

   fclose(yyin);

   return 0;

}
