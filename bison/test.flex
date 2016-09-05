%{
#include <stdlib.h>
#include "test.y.hh"
%}

%%

[-+*/\n()] {
    return *yytext;
}

[0-9]+ {
    yylval = strtol(yytext,0,10);
    return NUM;
}

%%
