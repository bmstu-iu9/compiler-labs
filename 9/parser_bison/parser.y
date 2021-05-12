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
int yylex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, yyscan_t scanner);
void yyerror(YYLTYPE *yyloc, yyscan_t scanner, long env[26], const char *msg);
%}

%%

program:
          statement ';' program
        | statement '.'
        ;

statement:
          expr
        | INPUT varlist
        | PRINT outlist
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

outlist:
          outlist ',' out
        | out
        ;

out:
          expr
          {
            printf("%ld\n", $expr);
          }
        ;

expr:
          IDENT '=' expr { $$ = env[$1 - 'a'] = $3; }
        | expr[sel] '?' '(' expr[neg] ',' expr[zero] ',' expr[pos] ')'
          {
            if ($sel < 0)
              $$ = $neg;
            else if ($sel == 0)
              $$ = $zero;
            else
              $$ = $pos;
          }
        | expr[L] '+' expr[R] { $$ = $L + $R; }
        | expr '-' expr { $$ = $1 - $3; }
        | expr '*' expr { $$ = $1 * $3; }
        | expr '/' expr { $$ = $1 / $3; }
        | '-' expr %prec UMINUS { $$ = -$2; }
        | '(' expr[in] ')' { $$ = $in; }
        | NUMBER { $$ = $NUMBER; }
        | IDENT { $$ = env[$1 - 'a']; }
        ;

%%

#define PROG \
  "input a, b;\n" \
  "print m = a-b ? (a, a, b), a + b - m, (a+b) / 2."

int main() {
  yyscan_t scanner;
  struct Extra extra;
  long env[26];

  init_scanner(PROG, &scanner, &extra);
  yyparse(scanner, env);
  destroy_scanner(scanner);
  return 0;
}


