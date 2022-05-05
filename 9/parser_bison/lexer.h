#ifndef LEXER_H_INCLUDED
#define LEXER_H_INCLUDED

#include <stdbool.h>
#include <stdio.h>

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void *yyscan_t;
#endif

struct Extra {
  bool continued;
  int cur_line;
  int cur_column;
  FILE *source;
};

void init_scanner(
    const char *source, yyscan_t *scanner, struct Extra *extra
);

void destroy_scanner(yyscan_t scanner, struct Extra *extra);

#endif /* LEXER_H_INCLUDED */
