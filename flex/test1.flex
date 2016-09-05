%{
#include <stdlib.h>

int line_num = 1;
%}
%x comment

DIGIT [0-9]
ID    [a-z][a-z0-9]*

%%
[\]"[^"]*[\"] {
    printf("[\"%.*s\"]\n",strlen(yytext)-2,yytext+1);
}

{DIGIT}+ {
    long int n = strtol(yytext,0,10);
    printf("%d\n",n+1);
}

"*/" printf("ERROR[%s][%d]\n",yytext,line_num);

"/*" BEGIN(comment);
<comment>"*"+"/" BEGIN(INITIAL);
<comment>[^*\n]*
<comment>"*"+[^/\n]*
<INITIAL,comment>"\n" ++line_num;

[^"*/\n0-9]* {
    printf("[%s]\n",yytext);       
}

%%

int main() {
    yylex();
    printf("Lines[%d]\n",line_num);
}