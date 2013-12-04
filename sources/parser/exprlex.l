D				[0-9]
ID                              [a-z\-\+\*%_\.][0-9A-Za-z\-\+\*%_']*
TYPEID                          [A-Z][a-zA-Z]*
STR_LIT                         \"(\\.|[^"])*\"
COMMENT				#[^\n]*\n

%start
%%

{COMMENT}			;

{D}+                            begin
				  yylval.yyInt64 := StrToInt(yytext);
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
data                            return(DATA_SYM);

{ID}				begin
				  yylval.yyString := yytext;
				  return(IDENT);
				end;

{TYPEID}			begin
				  yylval.yyString := yytext;
				  return(TYPEIDENT);
				end;

"()"                            return(UNIT_SYM);

" "             		;

\n				;
.				returnc(yytext[1]);
