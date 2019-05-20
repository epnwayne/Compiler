/*	Definition section */
%{
#include<stdio.h>
#include<string.h>
#define t_size 50
extern int yylineno;
extern int yylex();
extern char* yytext;   // Get current token from lex
extern char buf[256];  // Get current code line from lex

void yyerror(char* s);
/* Symbol table function - you can add new function if needed. */
int lookup_symbol();
void create_symbol();
void insert_symbol(int k, char nat[], int s);
void dump_symbol(int s);
void semantic_error();//detect semantic error
struct Table
{
    int flag;
    char name[20];
    char kind[20];
    char type[20];
    int scope;
    char attribute[30];
};

struct Table stb[t_size];//symble table
int stb_idx = 0;//symble table index
int Scope = 0;
int dflag = 0;//dflag = 1 if current line is a declaration statment
int uflag = 0;
int syerr = 0;//syntax error
char errmsg1[500] = {0};//error msg
char errmsg2[500] = {0};
/*
    uflag:
    1 -> check variable, 2-> check function
*/
char dname[20] = {0};//current declaration's name
%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    double f_val;
    char* string;
}

/* Token without return */
%token PRINT 
%token IF ELSE FOR WHILE
%token SEMICOLON
%token ADD SUB MUL DIV MOD INC DEC
%token ASGN ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%token MT LT MTE LTE EQ NE
%token LB RB LCB RCB QUOTA COMMA
%token C_COM CPP_COM
%token RET
%token TRUE FALSE

%nonassoc IFX
%nonassoc ELSE
/* Token with return, which need to sepcify type */
%token <string> ID
%token <string> INT FLOAT BOOL VOID STRING
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STR_CONST

/* Nonterminal with return, which need to sepcify type */
%type <string> type
%type <string> DCL_specify
%type <string> declaration
%type <string> func_parameter
%type <string> func_declaration
/*%type <f_val> stat
/*%type <f_val> initializer


/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%


program
    : program stat
    |

stat
    : declaration { if(!strcmp("", errmsg1))
                        insert_symbol(1, $1, Scope);
                  }
    | expression_stat
    | func_declaration {    insert_symbol(0, $1, Scope); 
                       }
    | print_func
    | return_stat
    | C_COM
    | CPP_COM
;

/* expression statement */
expression_stat
    : ID assignment expr SEMICOLON
    | postfix SEMICOLON
;
assignment
    : ASGN
    | ADDASGN
    | SUBASGN
    | MULASGN
    | DIVASGN
    | MODASGN
;

expr
    : expr comparison add_expr
    | add_expr
;
add_expr
    : add_expr addtion mul_expr
    | mul_expr
;
addtion
    : ADD
    | SUB
;
mul_expr
    : mul_expr multiplication postfix
    | postfix
;
multiplication
    : MUL
    | DIV
    | MOD
;
postfix
    : term postfix_operater
;
postfix_operater
    : 
    | postfix_operater INC
    | postfix_operater DEC
;
term
    : LB expr RB
    | ID { /*uflag = 1; strcpy(dname, $1);*/ semantic_error(1, $1);}
    | STR_CONST
    | number
    | func_call
;
number
    : I_CONST 
    | F_CONST
    | SUB I_CONST/* negative number */
    | SUB F_CONST
;

/* compound statement */
compound_stat
    : WHILE LB expr RB LCB while_content RCB
    | if_stmt
;
if_stmt
    : IF LB expr RB LCB while_content RCB  %prec IFX
    | IF LB expr RB LCB while_content RCB ELSE LCB while_content RCB
    | IF LB expr RB LCB while_content RCB ELSE if_stmt
;
comparison
    : MT
    | LT
    | MTE
    | LTE
    | EQ
    | NE
;
while_content
    : 
    | while_content content_stat
;
/* function declaration */
func_declaration
    : DCL_specify LB func_parameter RB LCB func_content RCB { sprintf($$, "%s %s", $1, $3); sprintf($3, "%s", ""); }
    | DCL_specify LB RB LCB func_content RCB {sprintf($$, "%s %s", $1, ""); }  
;

func_content
    :
    | func_content content_stat
    | func_content return_stat 
;
content_stat
    : compound_stat 
    | expression_stat
    | declaration {  if(!strcmp("", errmsg1))
                        insert_symbol(1, $1, Scope); 
                  }
    | print_func 
    | C_COM
    | CPP_COM

func_parameter
   /* :               { printf("hi");sprintf($$, "%s", "");printf("hi2");}*/
    : DCL_specify { if(!strcmp("", errmsg1))
                    {   
                        insert_symbol(2, $1, Scope+1);//insert parameter
                    }
                    sscanf($1, "%s", $$);
                  }
    | func_parameter COMMA DCL_specify {    if(!strcmp("", errmsg1))
                                                insert_symbol(2, $3, Scope+1);//insert parameter
                                            char t1[20] = {0};
                                            sscanf($3, "%s", t1); 
                                            sprintf($$, "%s, %s", $1, t1); 
                                       }
;

/* declaration */
declaration 
    : DCL_specify SEMICOLON { $$ = $1; }
    | DCL_specify ASGN expr SEMICOLON { $$ = $1; }
;

DCL_specify
    : type ID { sprintf($$, "%s %s", $1, $2);    
                //strcpy(dname, $2);
                semantic_error(0, $2);
              }
;

/* print function */
print_func
    : PRINT LB STR_CONST RB SEMICOLON
    | PRINT LB ID RB SEMICOLON { semantic_error(1, $3); }
;

/* return statement */
return_stat
    : RET expr SEMICOLON
    | RET SEMICOLON
    | RET TRUE SEMICOLON
    | RET FALSE SEMICOLON
;

/* function call */
func_call
    : ID LB func_call_parameter RB { /*uflag = 2; strcpy(dname, $1);*/ semantic_error(2, $1);}
    | ID LB RB { /*uflag = 2; strcpy(dname, $1);*/ semantic_error(2, $1); }
;
func_call_parameter 
    : expr
    | func_call_parameter COMMA expr
;
/* actions can be taken when meet the token or rule */
type
    : INT { $$ = $1; }
    | FLOAT { $$ = $1; }
    | BOOL  { $$ = $1; }
    | STRING { $$ = $1; }
    | VOID { $$ = $1; }
;

%%

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;
    create_symbol();

    yyparse();
    if(!syerr)
    {
        dump_symbol(0);
	    printf("\nTotal lines: %d \n", yylineno);
    }
    return 0;
}

void yyerror(char *s)
{
    printf("%d: %s\n", yylineno+1, buf);
    //check if there is also a semantic error
    ++yylineno;
    if(uflag == 1)
    {
        semantic_error(1, dname);
        uflag = 0;
    }
    else if(uflag == 2)
    {
        semantic_error(2, dname);
        uflag = 0;
    }
    if(dflag)
    {
        semantic_error(0, dname);
        dflag = 0;
    }
    --yylineno;
    if(strcmp("", errmsg1))
    {
        sprintf(errmsg1, "%s%s\n", errmsg1, buf);
        printf("%s%s", errmsg1, errmsg2);
        sprintf(errmsg1, "%s", "");
        sprintf(errmsg2, "%s", "");
    }
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno+1, buf);
    printf("| syntax error");
    printf("\n|-----------------------------------------------|\n\n");
    syerr = 1;
}

void create_symbol() 
{
    int i, j;
    for(i = 0; i < t_size; ++i)
    {
        memset(stb[i].name, 0, strlen(stb[i].name));
        memset(stb[i].kind, 0, strlen(stb[i].kind));
        memset(stb[i].type, 0, strlen(stb[i].type));
        memset(stb[i].attribute, 0, strlen(stb[i].attribute));
        stb[i].scope = 0;
        stb[i].flag = 0;
    }
}
void insert_symbol(int k, char nat[]/*name and type*/, int s) 
{
    //k: 0->func, 1->variable, 2-> parameter
    char tn[20] = {0};//name
    char tt[20] = {0};//type
    if(k == 0)
    {
        char att[30] = {0};//temp attrubute
        sscanf(nat, "%s %s", tt, tn);
        int sn = 0, i = 0;
        while(sn < 2)
        {
            if(nat[i] == ' ')
                ++sn;
            ++i;
        }
        strncpy(att, nat+i, strlen(nat));
        sprintf(stb[stb_idx].name, "%s", tn);
        sprintf(stb[stb_idx].type, "%s", tt);
        sprintf(stb[stb_idx].kind, "%s", "function");
        stb[stb_idx].scope = s;
        sprintf(stb[stb_idx].attribute, "%s", att);
    }
    else if(k == 1)
    {
        sscanf(nat, "%s %s", tt, tn);
        sprintf(stb[stb_idx].name, "%s", tn);
        sprintf(stb[stb_idx].type, "%s", tt);
        sprintf(stb[stb_idx].kind, "%s", "variable");
        stb[stb_idx].scope = s;
    }
    else
    {
        sscanf(nat, "%s %s", tt, tn);
        sprintf(stb[stb_idx].name, "%s", tn);
        sprintf(stb[stb_idx].type, "%s", tt);
        sprintf(stb[stb_idx].kind, "%s", "parameter");
        stb[stb_idx].scope = s;

    }

    ++stb_idx;
}

void semantic_error(int t, char name[])
{
    /* 
    *   t = 0 : check redeclare error
    *   t = 1 : check undeclare variable error
    *   t = 2 : check undeclare function error
    */
    if(t == 0)
    {
        if(lookup_symbol(name) == 2)
        {
            sprintf(errmsg1, "%s", "\n|-----------------------------------------------|\n");
            sprintf(errmsg1, "%s| Error found in line %d: ", errmsg1, yylineno+1);
            sprintf(errmsg2, "| Redeclared variable %s", name);
            sprintf(errmsg2, "%s\n|-----------------------------------------------|\n\n", errmsg2);
        }
    }
    else if(t == 2)
    {
        char temp[100] = {0};
        if(lookup_symbol(name) == 1)
        {
            sprintf(errmsg1, "%s", "\n|-----------------------------------------------|\n");
            sprintf(errmsg1, "%s| Error found in line %d: ", errmsg1, yylineno+1);
            sprintf(errmsg2, "| Undeclared function %s", name);
            sprintf(errmsg2, "%s\n|-----------------------------------------------|\n\n", errmsg2);
        }
    }
    else if(t == 1)
    {
        if(lookup_symbol(name) == 1)
        {
            sprintf(errmsg1, "%s", "\n|-----------------------------------------------|\n");
            sprintf(errmsg1, "%s| Error found in line %d: ", errmsg1, yylineno+1);
            sprintf(errmsg2, "| Undeclared variable %s", name);
            sprintf(errmsg2, "%s\n|-----------------------------------------------|\n\n", errmsg2);
        }
    }
}
int lookup_symbol(char check[]) 
{
    int i, equal_num = 0, larger_num = 0;
    for(i = 0; i < stb_idx; ++i)
    {
        if(!strcmp(stb[i].name, check) && Scope == stb[i].scope)
            equal_num++;
        if(!strcmp(stb[i].name, check) && Scope >= stb[i].scope)
            larger_num++;
    }
    if(equal_num > 0)
        return 2;
    else if(larger_num < 1)
        return 1;
    else
        return 0;
}
void dump_symbol(int s) {
    int i, f = 0;
    for(i = 0; i < stb_idx; ++i)
        if(stb[i].scope == s)
        {    
            printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n",
                    "Index", "Name", "Kind", "Type", "Scope", "Attribute");
            f = 1;
            break;
        }
    int Index = 0;
    if(s == 0)
    {
        for(i = 0; i < stb_idx; ++i)
        {
            if(!stb[i].flag)
            {
                printf("%-10d%-10s%-12s%-10s%-10d%s",
                        Index, stb[i].name, stb[i].kind, stb[i].type, stb[i].scope, stb[i].attribute
                        );
                stb[i].flag = 1;
                ++Index;
                printf("\n");
            }
        }
    }
    else
    {
        for(i = 0; i < stb_idx; ++i)
        {
            if(!stb[i].flag && stb[i].scope == s && stb[i].scope != 0)
            {
                printf("%-10d%-10s%-12s%-10s%-10d%s",
                        Index, stb[i].name, stb[i].kind, stb[i].type, stb[i].scope, stb[i].attribute
                        );
                stb[i].flag = 1;
                ++Index;
                printf("\n");
            }
        }
    }
    if(f)
        printf("\n");
}
