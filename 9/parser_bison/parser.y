%{
#include <stdio.h>
#include <stdlib.h>
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

%right '='
%right '?'
%left '+' '-'
%left '*' '/'
%left UMINUS

%token INPUT PRINT
%token <ident> IDENT
%token <num> NUMBER

%type <num> expr

%{
int yylex(
    YYSTYPE *yylval_param, YYLTYPE *yylloc_param, yyscan_t scanner
);

void yyerror(
    YYLTYPE *loc, yyscan_t scanner, long env[26], const char *msg
);
%}

%%

program:
        statement ';' program
      | statement '.'
      ;

statement:
        expr
      | INPUT varlist
      | PRINT exprlist
      ;

varlist:
        varlist ',' var
      | var
      ;

var:
        IDENT
        {
          printf("%c = ", $1);
          scanf("%ld", &env[$1 - 'a']);
        }
      ;

exprlist:
        exprlist ',' out
      | out
      ;

out:
        expr
        {
          printf("%ld\n", $expr);
        }
      ;

expr:
        IDENT '=' expr  { $$ = env[$1 - 'a'] = $3; }
      | expr[sel] '?' '(' expr[neg] ',' expr[zero] ',' expr[pos] ')'
        {
          if ($sel < 0) {
            $$ = $neg;
          } else if ($sel == 0) {
            $$ = $zero;
          } else {
            $$ = $pos;
          }
        }
      | expr[L] '+' expr[R]  { $$ = $L + $R; }
      | expr '-' expr  { $$ = $1 - $3; }
      | expr '*' expr  { $$ = $1 * $3; }
      | expr '/' expr  { $$ = $1 / $3; }
      | '-' expr        %prec UMINUS { $$ = -$2; }
      | '(' expr[in] ')'   { $$ = $in; }
      | NUMBER         { $$ = $NUMBER; }
      | IDENT { $$ = env[$IDENT - 'a']; }
      ;

%%

int main(int argc, char *argv[]) {
  yyscan_t scanner;
  struct Extra extra;
  long env[26];

  if (argc != 2) {
    printf("Expected filename in commandline\n");
    return EXIT_FAILURE;
  }

  init_scanner(argv[1], &scanner, &extra);
  yyparse(scanner, env);
  destroy_scanner(scanner, &extra);
  return EXIT_SUCCESS;
}
