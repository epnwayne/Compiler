/* Definaition section */
%{
	#include <stdio.h>
	#include <stdlib.h>
    #include "y.tab.h"
    /*save text to buf and clr buf*/
    void save();
    void clear();

    extern YYSTYPE yylval;
    extern void dump_symbol();
    extern void semantic_error();
    extern int Scope;
    extern char dname[20];
    extern char errmsg1[500];
    extern char errmsg2[500];
    char buf[256] = {0};
    int idx = 0;
%}

/* Define regular expression label */

letter [a-zA-Z]
digit [0-9]
id {letter}+({letter}|{digit})*
number {digit}+
integer {digit}+
float {digit}*"."{digit}+
comment "/*"([^*]|\n|"*"+[^/])*"*"+"/"

%option yylineno
%x COMMENT STRING_STATE
header file generated by bison
/* Rules section */
%%

 /* Arithmetic */
"+"     { save(); return ADD; }
"-"		{ save(); return SUB; }
"*"     { save(); return MUL; }
"/"     { save(); return DIV; }
"%"     { save(); return MOD; }
"++"    { save(); return INC; }
"--"    { save(); return DEC; }

 /* Relational */
">"     { save(); return MT; }
"<"     { save(); return LT; }
">="    { save(); return MTE; }
"<="    { save(); return LTE; }
"=="    { save(); return EQ; }
"!="    { save(); return NE; }


"["     { printf("%s \t LSB\n", yytext); }
"]"     { printf("%s \t RSB\n", yytext); }
","     { save(); return COMMA;}
";"     { save(); return SEMICOLON;}
"\""    { save(); return QUOTA;}
"print" { save(); return PRINT;}
"if"    { save(); return IF; }
"else"  { save(); return ELSE; }
"for"   { printf("%s \t FOR\n", yytext); }
"while" { save(); return WHILE; }
 
 /* Assignment */
"="     { save(); return ASGN; }
"+="    { save(); return ADDASGN; }
"-="    { save(); return SUBASGN; }
"*="    { save(); return MULASGN; }
"/="    { save(); return DIVASGN; }
"%="    { save(); return MODASGN; }
 
 /* Logical */
"&&"    { printf("%s \t AND\n", yytext); }
"||"    { printf("%s \t OR\n", yytext); }
"!"     { printf("%s \t NOT\n", yytext); }

"("     { save(); return LB;}
")"     { save(); return RB;}
"{"     { save(); ++Scope; return LCB;}
"}"     { save(); --Scope; return RCB;}

 /* Declaration Keywords */
"void"  { save(); yylval.string = strdup(yytext); return VOID; }
"bool"  { save(); yylval.string = strdup(yytext); return BOOL; }
"string"    { save(); yylval.string = strdup(yytext); return STRING; }
"int"   { save(); yylval.string = strdup(yytext); return INT; }
"float" { save(); yylval.string = strdup(yytext); return FLOAT; }

 /* boolean Keywords */
"true"  { save(); return TRUE; }
"false" { save(); return FALSE; }

"return"    { save(); return RET; }
"continue"  { printf("%s \t CONT\n", yytext); }
"break" { printf("%s \t BREAK\n", yytext); }

"//".* { save(); return CPP_COM; }

"\""(\\.|[^"])+"\""  { save(); return STR_CONST; }
 
 /* C type Comment */
"/*"				{ save(); BEGIN COMMENT; }
<COMMENT>[^*\n]+	{ save(); }
<COMMENT>\n			{ save(); printf("%d: %s", yylineno, buf); clear();}
<COMMENT>"*"		{ save(); }
<COMMENT>"*/"		{ save(); BEGIN INITIAL; } 
               
{integer}   { save(); sscanf(yytext, "%d", &yylval.i_val); return I_CONST;}
{float}     { save(); sscanf(yytext, "%lf", &yylval.f_val); return F_CONST;}
{id}	{ save(); yylval.string = strdup(yytext); return ID;}

\n		{ 
          if(strlen(buf) > 0)
            printf("%d: %s\n", yylineno, buf); 
          else
            printf("%d:\n", yylineno);

          /* solve semantic error*/
          if(strcmp(errmsg1, ""))
          {
              sprintf(errmsg1, "%s%s\n", errmsg1, buf);
              printf("%s%s", errmsg1, errmsg2);
              sprintf(errmsg1, "%s", "");
              sprintf(errmsg2, "%s", "");
          }
          /* print symbol table */
          if(buf[strlen(buf)-1] == '}')
          {
                dump_symbol(Scope+1);
          }
          clear();
        }

" "     { buf[idx] = ' '; ++idx;}


%%
void clear()
{
    idx = 0;
    memset(buf, 0 , 256);
}
void save()
{
    int t = yyleng;
    int i = 0;
    while(t)
    {
        buf[idx] = yytext[i];
        ++idx;
        ++i;
        --t;
    }
    return;
}


/*	C Code section */
int yywrap(void)
{
    return 1;
}

/*
int main(int argc,char *argv[])
{
	yyin = fopen(argv[1],"r");
	yylex();
	printf("\nParse over, the line number is %d.\n\n", line_num);
	printf("comment: %d lines\n\n", com_num);

	return 0;
}
*/