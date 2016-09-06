%{
    #include <stdio.h>
    #include <math.h>
    #include "test.flex.hh"
    extern void yyerror(char const *);
%}

%initial-action {
    printf("initial-action\n");
}

%union {
    double value;
}

%token NUM
%token CALC

%type <value> line, exp, NUM

%left '+' '-'
%left '*' '/'
%left '^'

%%
input:

|   input line
;

line:
    '\n'
| CALC exp '\n' { printf("%g\n", $2); }
| error '\n' { yyerrok; }
;

exp:
    NUM          { $$ = $1; }
|   '(' exp ')'  { $$ = $2; }    
|   exp '+' exp  { $$ = $1 + $3; }
|   exp '-' exp  { $$ = $1 - $3; }
|   exp '*' exp  { $$ = $1 * $3; }
|   exp '/' exp  { $$ = $1 / $3; }
|   exp '^' exp  { $$ = pow($1,$3); }
|   '-' exp      { $$ = -$2; }
;

%%

void yyerror(char const *s) {
    fprintf(stderr, "yyerror[%s]\n", s);
}

int main(void) {
    return yyparse();
}