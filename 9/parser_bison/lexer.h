
#ifndef _LEXER_H_INCLUDED
#define _LEXER_H_INCLUDED

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

struct Extra {
	int continued;
	int cur_line;
	int cur_column;
};

void init_scanner(char *program, yyscan_t *scanner, struct Extra *extra);
void destroy_scanner(yyscan_t scanner);

#endif
