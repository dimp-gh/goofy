L				[A-Za-z]
D				[0-9]
ID                              {L}+

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

{ID}				begin
				  yylval.yyString := yytext;
				  return(IDENT);
				end;

" "             		;

.				|
\n				returnc(yytext[1]);
