%option reentrant noyywrap bison-bridge bison-locations
%option extra-type="struct Extra *"

%{

#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "y.tab.h"

#define YY_USER_ACTION \
  { \
    struct Extra *extra = yyextra; \
    /* В C++: ... = static_cast<Extra*>(yyextra) */ \
    if (! extra->continued) { \
      yylloc->first_line = extra->cur_line; \
      yylloc->first_column = extra->cur_column; \
    } \
    extra->continued = false; \
    for (int i = 0; i < yyleng; ++i) { \
      if (yytext[i] == '\n') { \
        extra->cur_line++; \
        extra->cur_column = 1; \
      } else { \
        extra->cur_column++; \
      } \
    } \
    \
    yylloc->last_line = extra->cur_line; \
    yylloc->last_column = extra->cur_column; \
  }

void yyerror(
    YYLTYPE *loc, yyscan_t scanner, long env[26], const char *msg
) {
  printf(
    "Error (%d, %d): %s\n", loc->first_line, loc->first_column, msg
  );
}

%}

DIGIT [0-9]
NUMBER {DIGIT}+

%%

[\n\t ]+

"+"             return '+';
"-"             return '-';
"*"             return '*';
"/"             return '/';
"("             return '(';
")"             return ')';
"."             return '.';
";"             return ';';
","             return ',';
"="             return '=';
"?"             return '?';

[a-z]           {
                  yylval->ident = yytext[0];
                  return IDENT;
                }

{NUMBER}        {
                  yylval->num = atol(yytext);
                  return NUMBER;
                }

input           return INPUT;
print           return PRINT;

%%

void init_scanner(
    const char *source, yyscan_t *scanner, struct Extra *extra
) {
  extra->continued = false;
  extra->cur_line = 1;
  extra->cur_column = 1;
  extra->source = fopen(source, "rt");

  if (! extra->source) {
    printf("Can't open file '%s'\n", source);
    exit(EXIT_FAILURE);
  }

  yylex_init(scanner);
  yylex_init_extra(extra, scanner);
  yyset_in(extra->source, *scanner);
}

void destroy_scanner(yyscan_t scanner, struct Extra *extra) {
  yylex_destroy(scanner);
  fclose(extra->source);
}
