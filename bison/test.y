%{
    #include <stdio.h>
    #include "test.flex.hh"
    extern void yyerror(char const *);
%}

%initial-action {
    printf("initial-action\n");
}

%token NUM

%left '+' '-'
%left '*' '/'

%%
input:

|   input line
;

line:
    '\n'
|   exp '\n' { printf("%d\n", $1); }
;

exp:
    NUM         { $$ = $1; }
|   '(' exp ')' { $$ = $2; }    
|   exp '+' exp { $$ = $1 + $3; }
|   exp '-' exp { $$ = $1 - $3; }
|   exp '*' exp { $$ = $1 * $3; }
|   exp '/' exp { $$ = $1 / $3; }
|   '-' exp     { $$ = -$2; }
;

%%

void yyerror(char const *s) {
    fprintf(stderr, "[%s]\n", s);
}

int main(void) {
    return yyparse();
}