
%{
#include <stdio.h>
#include "lexer.h"
%}

%define api.pure
%locations
%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner}
%parse-param {long env[26]}

%union {
	char ident;
	long num;
}

%right ASSIGN
%right QMARK
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left UMINUS
%token INPUT PRINT LPAREN RPAREN COMMA SEMICOLON DOT
%token <ident> IDENT
%token <num> NUMBER

%type <num> expr

%{
int yylex(YYSTYPE * yylval_param, YYLTYPE * yylloc_param , yyscan_t scanner);
void yyerror(YYLTYPE *yylloc, yyscan_t scanner, long env[26], char *msg);
%}

%%

program:	statement SEMICOLON program
	|	statement DOT;

statement:	expr
	|	INPUT varlist
	|	PRINT outlist
	;

varlist:	varlist COMMA var
	|	var
	;

var:		IDENT				{
							printf("%c = ", $1);
							scanf("%ld", &env[$1-'a']);
						}
	;

outlist:	outlist COMMA out
	|	out
	;

out:		expr				{ printf("%ld\n", $1); }

expr:		IDENT ASSIGN expr		{ $$ = env[$1-'a'] = $3; }
	|	expr QMARK LPAREN expr COMMA expr COMMA expr RPAREN
						{
							if ($1 < 0) $$ = $4;
							else $$ = $1 ? $8 : $6;
						}
	|	expr PLUS expr			{ $$ = $1 + $3; }
	|	expr MINUS expr			{ $$ = $1 - $3; }
	|	expr MULTIPLY expr		{ $$ = $1 * $3; }
	|	expr DIVIDE expr		{ $$ = $1 / $3; }
	|	MINUS expr %prec UMINUS		{ $$ = -$2; }
	|	LPAREN expr RPAREN		{ $$ = $2; }
	|	NUMBER				{ $$ = $1; }
	|	IDENT				{ $$ = env[$1-'a']; }
	;

%%

#define PROG \
	"input a, b;\n" \
	"print m = a-b ? (a, a, b), a+b-m, (a+b)/2."

int main()
{
	yyscan_t scanner;
	struct Extra extra;
	long env[26];
	init_scanner(PROG, &scanner, &extra);
	yyparse(scanner, env);
	destroy_scanner(scanner);
	return 0;
}
