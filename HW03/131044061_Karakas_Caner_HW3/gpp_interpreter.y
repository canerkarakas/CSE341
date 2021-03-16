%{ 
	/* Definition section */
	#include <string.h>
	#include <stdio.h>
    #include <stdlib.h>
    #include <stdarg.h>
    #include <math.h>
    #include "y.tab.h"
    int yyparse(void);
    int yylex(void);
    void yyerror(char *);
	char* equal1 (char *expr1, char *expr2);
	char* equal2 (int expr1, int expr2);
	char* and (char *expr1, char *expr2);
	char* or (char *expr1, char *expr2);
	char* less (int expr1, int expr2);
	char* not (char *expr);
	void displayList();
	void emptyList();
	void addingList(int number); 
	int* list();
	void doIF1(char* expr);
	
	
%} 

%union
{
	char *str;
	int integerVal;
	int *integerList;
}

%token COMMENT

%token KW_AND

%token KW_OR

%token KW_NOT

%token KW_EQUAL

%token KW_LESS

%token KW_nil

%token KW_LIST

%token KW_APPEND

%token KW_CONCAT

%token KW_SET

%token KW_DEFFUN

%token KW_FOR

%token KW_IF

%token KW_EXIT

%token KW_LOAD

%token KW_DISP

%token KW_TRUE

%token KW_FALSE

%token OP_STR

%token OP_OP

%token OP_CP

%token OP_COMMA

%token OP_PLUS

%token OP_MINUS

%token OP_DIV

%token OP_MULT

%token OP_DBLMULT

%token OP_OC

%token <str> ID

%token <integerVal> VALUE

%type <integerVal> LISTVALUE

%type <integerVal> EXPI

%type <str> EXPB

%type <integerList> EXPLISTI

%type <integerVal> VALUES

%type <str> TRUE

%type <str> FALSE

%type <str> ID1

%start basla

/* Rule Section */
%% 

basla:/* empty */ { printf("> ");}
        | basla input
        ;

input:  EXPB {printf("%s \n", $1);} 
        | EXPI {printf("%d \n", $1);} 
        | EXPLISTI {displayList(); emptyList();}
        | DEFFUN
        | IF
        | SET
        | LOAD
        | DISP
        | EXIT;

EXPB:     OP_OP KW_AND EXPB EXPB OP_CP { $$ = and($3, $4); }
        | OP_OP KW_OR EXPB EXPB OP_CP { $$ = or($3, $4); }
        | OP_OP KW_NOT EXPB OP_CP { $$ = not($3); }
        | OP_OP KW_EQUAL EXPB EXPB OP_CP { $$ = equal1($3, $4); }
        | OP_OP KW_EQUAL EXPI EXPI OP_CP { $$ = equal2($3, $4); }
        | OP_OP KW_LESS EXPI EXPI OP_CP {$$ = less($3, $4);}
        | TRUE
        | FALSE
        ;
        
TRUE:  KW_TRUE;

FALSE: KW_FALSE;

EXPI: VALUE {$$=$1;}
        |OP_OP OP_PLUS EXPI EXPI OP_CP {$$ = $3 + $4;}
        | OP_OP OP_MINUS EXPI EXPI OP_CP {$$ = $3 - $4;}
        | OP_OP OP_DIV EXPI EXPI OP_CP {$$ = $3 / $4;}
        | OP_OP OP_MULT EXPI EXPI OP_CP {$$ = $3 * $4;}
        | OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = $3 * $4;}
        | VALUES
        ;
        
EXPLISTI: 
         OP_OP KW_APPEND EXPI EXPLISTI OP_CP {addingList($3);}
        | OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP
        | OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP
        | LISTVALUE
        ;
        
LISTVALUE:   OP_OC OP_OP VALUES OP_CP
            | OP_OP KW_LIST VALUES OP_CP
            | OP_OC OP_OP OP_CP ;
            
VALUES: VALUES VALUE {addingList($2);} | VALUE {addingList($1);};

IF: OP_OP KW_IF EXPB EXPLISTI OP_CP {doIF1($3);};

SET: OP_OP KW_SET ID EXPI OP_CP {printf("%d \n", $4);};
    
DEFFUN: OP_OP KW_DEFFUN ID OP_OP ID OP_CP input OP_CP;

LOAD: OP_OP KW_LOAD ID OP_CP {printf("\"%s\"\n", $3);};

DISP: OP_OP KW_DISP ID OP_CP {printf("\"%s\"\n", $3);};

EXIT: KW_EXIT {exit(0);} | OP_OP KW_EXIT OP_CP {exit(0);};

%% 

int listArray[1000];
int _index=0;

char* equal1(char *expr1, char *expr2){
    if ( (strcmp(expr1, expr2) == 0))
        return "true";
    return "false";
}

char* equal2(int expr1, int expr2){
   if(expr1 == expr2)
        return "true";
   return "false";
}

char* less(int expr1, int expr2){
   if(expr1 < expr2)
        return "true";
   return "false";
}

char* and(char *expr1, char *expr2){
    if ( (strcmp(expr1, "false") == 0) || (strcmp(expr2, "false") == 0))
        return "false";
    return "true";
}

char* or(char *expr1, char *expr2){
    if ( (strcmp(expr1, "false") == 0) && (strcmp(expr2, "false") == 0))
        return "false";
    return "true";
}

char* not(char *expr){
    if ( (strcmp(expr, "false") == 0))
        return "true";
    return "false";
}

void yyerror(char *s){
    fprintf(stderr, "%s\n", s);
}

void displayList() {
    int i = 0;
    printf("( ");
    while(i<_index) {
        printf("%d ",listArray[i]);
        i++;
    }
    printf(")\n");
}

void emptyList(){
    int *freeArray = (int*) malloc(sizeof(int)*1000);
    *listArray = freeArray;
    _index = 0;
}

void addingList(int number) {
    listArray[_index] = number;
    _index = _index + 1;
}

int* list(){
    return listArray;
}

void doIF1(char* expr){
    if(strcmp(expr,"true") == 0)
        displayList();
    else
        printf("false");
    
    emptyList();
}

int main(void){
    yyparse();
    return 0;
}









