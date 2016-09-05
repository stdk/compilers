%option stack
/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;
char *string_buf_end = string_buf + sizeof(string_buf)/sizeof(*string_buf);
int string_error = 0;

#define STRING_ERROR(msg) do{\
  if(!string_error) {\
    string_error = 1;\
    cool_yylval.error_msg = (msg);\
    return ERROR;\
  }\
}while(0)

#define STRING_ADD_CHAR(c) do{\
  if(string_buf_ptr < string_buf_end-1) {\
    *string_buf_ptr++ = c;\
  } else {\
    STRING_ERROR("String constant too long.");\
  }\
}while(0)

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

%}
%x COMMENT STRING

%%

--[^\n]*

"(*" {
  //printf("[deeper][%s]\n",yytext);
  yy_push_state(COMMENT);
}
<COMMENT>[(]+[*] {
  //printf("[deeper][%s]\n",yytext);
  yy_push_state(COMMENT);
}
<COMMENT>[*]+[)] {
  //printf("[up][%s]\n",yytext);
  yy_pop_state();
}
<COMMENT>[^(*\n]* //printf("[skip][%s]\n",yytext);
<COMMENT>[(]+[^*\n]* //printf("[skip][%s]\n",yytext);
<COMMENT>[*]+[^*)\n]* //printf("[skip][%s]\n",yytext);

<COMMENT><<EOF>> {
  cool_yylval.error_msg = "EOF in comment";
  yy_pop_state();
  return ERROR;
}
<INITIAL,COMMENT>[\n] {
  //printf("[skip][\\n]\n");
  ++curr_lineno;
}

[-+*=/<.~,;:()@{}] return yytext[0];
"=>" return DARROW;
"<-" return ASSIGN;
"<=" return LE;
(?i:class) return CLASS;
(?i:inherits) return INHERITS;
(?i:if) return IF;
(?i:then) return THEN;
(?i:else) return ELSE;
(?i:fi) return FI;
(?i:while) return WHILE;
(?i:loop) return LOOP;
(?i:pool) return POOL;
(?i:let) return LET;
(?i:in) return IN;
(?i:case) return CASE;
(?i:of) return OF;
(?i:esac) return ESAC;
(?i:new) return NEW;
(?i:isvoid) return ISVOID;
(?i:not) return NOT;

[0-9]+ {
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}

[\"] {
  string_buf_ptr = string_buf;
  string_error = 0;
  BEGIN(STRING);
}
<STRING>[\"] {
  BEGIN(INITIAL);
  if(!string_error) {
    cool_yylval.symbol = stringtable.add_string(string_buf,string_buf_ptr-string_buf);
    return STR_CONST;
  }
}

<STRING><<EOF>> {
  BEGIN(INITIAL);
  STRING_ERROR("EOF in string constant.");
}
<STRING>[\0] STRING_ERROR("String contains null character.");
<STRING>"\\\0" STRING_ERROR("String contains escaped null character.");
<STRING>[\n] {
  ++curr_lineno;
  BEGIN(INITIAL);
  STRING_ERROR("Unterminated string constant.");
}

<STRING>"\\\n" {
  STRING_ADD_CHAR('\n');
  ++curr_lineno;
} 

<STRING>"\\b" STRING_ADD_CHAR('\b');
<STRING>"\\t" STRING_ADD_CHAR('\t');
<STRING>"\\n" STRING_ADD_CHAR('\n');
<STRING>"\\f" STRING_ADD_CHAR('\f');
<STRING>[\\][^\n\0] STRING_ADD_CHAR(yytext[1]);
<STRING>. STRING_ADD_CHAR(*yytext);


t(?i:rue)|f(?i:alse) {
  cool_yylval.boolean = *yytext == 't';
  return BOOL_CONST;
}

[a-z][_a-zA-Z0-9]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

[A-Z][_a-zA-Z0-9]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

[ \f\r\t\v]+

"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return ERROR;
}

. {
  cool_yylval.error_msg = yytext;
  return ERROR;
}

%%
