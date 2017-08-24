%token IF ELSE PASS NRO
%nonassoc THEN
%left ELSE
%%
stm: IF '(' exp ')' %prec THEN stm
    | IF '(' exp ')' stm ELSE stm
    | PASS
    ;
exp: NRO
    ;
%%
