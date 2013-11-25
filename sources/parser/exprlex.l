D				[0-9]
ND                              [A-Za-z\-\+\*/|%]
ID                              [A-Za-z\-\+\*/|%][0-9A-Za-z\-\+\*/|%]*

%start
%%

{D}+                            begin
				  yylval.yyInteger := StrToInt(yytext);
				  return(NUM);
				end;

fn                              return(LAMBDA_SYM);
=>                              return(LAMBDA_ARROW_SYM);
let                             return(LET_SYM);
letrec				return(LETREC_SYM);
=				return(EQUALS_SYM);
in				return(IN_SYM);
if                              return(IF_SYM);
then                            return(THEN_SYM);
else                            return(ELSE_SYM);

{ID}				begin
				  yylval.yyString := yytext;
				  return(IDENT);
				end;

"()"                            return(UNIT_SYM);

" "             		;

.				|
\n				returnc(yytext[1]);
