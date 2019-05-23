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
void insert_symbol(int k, char name[], char type[], char attr[], int s, int fdcl);
void dump_symbol(int s);
void semantic_error();//detect semantic error
struct Table
{
    int flag;
    int func_fdcl;
    char name[20];
    char kind[20];
    char type[20];
    int scope;
    char attribute[30];
};

struct Table stb[t_size];//symble table
int stb_idx = 0;//symble table index
int Scope = 0;
int se_flag = 0;//semantic error check flag 0-> rv, 3-> rf, 1-> uv, 2-> uf 
int syerr = 0;//syntax error
int rf_flag = 0;//check if there is a rf error
char errmsg1[500] = {0};//error msg
char errmsg2[500] = {0};
char tn[30] = {0};//current declaration variable name
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
%type <string> declaration
%type <string> func_parameter
%type <string> func_declaration

/*%type <f_val> stat*/
/*%type <f_val> initializer*/


/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%


program
    : program stat
    |

stat
    : declaration 
    | expression_stat
    | func_declaration
    | print_func
    | return_stat
    | C_COM
    | CPP_COM
;
/* expression statement */
expression_stat
    : ID assignment expr SEMICOLON { se_flag = 1; sprintf(tn, "%s", $1); semantic_error(1, $1); }
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
    | ID { se_flag = 1; sprintf(tn, "%s", $1); semantic_error(1, $1);}
    | STR_CONST
    | number
    | func_call
;
number
    : I_CONST 
    | F_CONST
    | SUB I_CONST
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
    : type ID LB func_parameter RB LCB func_content RCB { semantic_error(3, $2); insert_symbol(0, $2, $1, $4, Scope, 0); }
    | type ID LB RB LCB func_content RCB { semantic_error(3, $2); insert_symbol(0, $2, $1, "", Scope, 0); }  
    | type ID LB func_parameter RB SEMICOLON { rf_flag = 1; semantic_error(3, $2); rf_flag = 0; insert_symbol(0, $2, $1, $4, Scope, 1); }
    | type ID LB RB SEMICOLON { rf_flag = 1; semantic_error(3, $2); rf_flag = 0; insert_symbol(0, $2, $1, "", Scope, 1); }
;

func_content
    :
    | func_content content_stat
    | func_content return_stat 
;
content_stat
    : compound_stat 
    | expression_stat
    | declaration
    | print_func 
    | C_COM
    | CPP_COM

func_parameter
    : type ID { insert_symbol(2, $2, $1, "", Scope+1, 0); sprintf($$, "%s", $1); }
    | func_parameter COMMA type ID { insert_symbol(2, $4, $3, "", Scope+1, 0); sprintf($$, "%s, %s", $1, $3); }
;

/* declaration */
declaration 
    : type ID SEMICOLON { se_flag = 0; sprintf(tn, "%s", $2); semantic_error(0, $2); insert_symbol(1, $2, $1, "", Scope, 0); }
    | type ID ASGN expr SEMICOLON { se_flag = 0; sprintf(tn, "%s", $2); semantic_error(0, $2); insert_symbol(1, $2, $1, "", Scope, 0); }
;

/* print function */
print_func
    : PRINT LB STR_CONST RB SEMICOLON
    | PRINT LB ID RB SEMICOLON { se_flag = 1; sprintf(tn, "%s", $3); semantic_error(1, $3); }
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
    : ID LB func_call_parameter RB { se_flag = 2; sprintf(tn, "%s", $1); semantic_error(2, $1); }
    | ID LB RB { se_flag = 2; sprintf(tn, "%s", $1); semantic_error(2, $1); }
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
    if(se_flag == 1)
    {
        semantic_error(1, tn);
        se_flag = 0;
    }
    else if(se_flag == 2)
    {
        semantic_error(2, tn);
        se_flag = 0;
    }
    else if(se_flag == 0)
    {
        semantic_error(0, tn);
        se_flag = 0;
    }
    else if(se_flag == 3)
    {
        semantic_error(3, tn);
        se_flag = 0;
    }
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
void insert_symbol(int k, char name[], char type[], char attr[], int s, int func_fdcl) 
{
    int i = 0, flag = 0;
    for(i = 0; i < stb_idx; ++i)
    {
        if(!strcmp(name, stb[i].name))
            return;
    }
    if(!strcmp("", errmsg1))
    {
        //k: 0->func, 1->variable, 2-> parameter
        if(k == 0 /*&& func_fdcl == 0*/)
        {
            sprintf(stb[stb_idx].name, "%s", name);
            sprintf(stb[stb_idx].type, "%s", type);
            sprintf(stb[stb_idx].kind, "%s", "function");
            stb[stb_idx].scope = s;
            sprintf(stb[stb_idx].attribute, "%s", attr);
            stb[stb_idx].flag = 0;
            stb[stb_idx].func_fdcl = func_fdcl;
            ++stb_idx;
        }
        if(k == 1)
        {
            sprintf(stb[stb_idx].name, "%s", name);
            sprintf(stb[stb_idx].type, "%s", type);
            sprintf(stb[stb_idx].kind, "%s", "variable");
            stb[stb_idx].scope = s;
            stb[stb_idx].flag = 0;
            stb[stb_idx].func_fdcl = func_fdcl;
            ++stb_idx;
        }
        if(k == 2)
        {
            sprintf(stb[stb_idx].name, "%s", name);
            sprintf(stb[stb_idx].type, "%s", type);
            sprintf(stb[stb_idx].kind, "%s", "parameter");
            stb[stb_idx].scope = s;
            stb[stb_idx].flag = 0;
            stb[stb_idx].func_fdcl = func_fdcl;
            ++stb_idx;
        }
    }
}

void semantic_error(int t, char name[])
{
    /* 
    *   t = 0 : check redeclare variable error
    *   t = 3 : check redefine and redeclare function error
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
    else if(t == 3)
    {
        if(lookup_symbol(name) == 2)
        {
            sprintf(errmsg1, "%s", "\n|-----------------------------------------------|\n");
            sprintf(errmsg1, "%s| Error found in line %d: ", errmsg1, yylineno+1);
            sprintf(errmsg2, "| Redeclared function %s", name);
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
        if(rf_flag)
        {
            if(!strcmp(stb[i].name, check) && Scope == stb[i].scope && stb[i].flag == 0)
                equal_num++;
        }
        else
        {
            if(!strcmp(stb[i].name, check) && Scope == stb[i].scope && stb[i].flag == 0 && stb[i].func_fdcl == 0)
                equal_num++;
            if(!strcmp(stb[i].name, check) && Scope >= stb[i].scope)
                larger_num++;
        }
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
            //printf("name = %s, fdcl = %d, scope = %d\n", stb[i].name, stb[i].func_fdcl, stb[i].scope);
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
            //printf("name = %s, fdcl = %d, scope = %d\n", stb[i].name, stb[i].func_fdcl, stb[i].scope);
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
