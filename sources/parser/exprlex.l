D				[0-9]
ID                              [A-Za-z\-\+\*%_\.][0-9A-Za-z\-\+\*%_']*
STR_LIT                         \"(\\.|[^"])*\"
COMMENT				#[^\n]*\n

%start
%%

{COMMENT}			;

{D}+                            begin
				  yylval.yyInteger := StrToInt(yytext);
				  return(NUM);
				end;

{STR_LIT}                       begin
                                  yylval.yyString := yytext;
                                  return(STRINGLIT);
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
true                            return(TRUE_SYM);
false                           return(FALSE_SYM);
case                            return(CASE_SYM);
of                              return(OF_SYM);
->                              return(CASE_ARROW_SYM);
end                             return(END_SYM);
fun                             return(FUN_SYM);
val                             return(VAL_SYM);
module                          return(MODULE_SYM);
where                           return(WHERE_SYM);
do                              return(DO_SYM);

{ID}				begin
				  yylval.yyString := yytext;
				  return(IDENT);
				end;

"()"                            return(UNIT_SYM);

" "             		;

\n				;
.				returnc(yytext[1]);
