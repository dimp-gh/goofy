D				[0-9]
ID                              [A-Za-z][A-Za-z0-9\-]*

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

" "             		;

.				|
\n				returnc(yytext[1]);
