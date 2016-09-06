%{
#include <stdio.h>
#include "test.y.hh"
%}

%%

[-+*/\n()^] {
    return *yytext;
}

calc {
    return CALC;
}

[0-9]+([.][0-9]*)? {
    sscanf(yytext,"%lf",&yylval.value);
    return NUM;
}

%%
