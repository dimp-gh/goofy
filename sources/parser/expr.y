%{unit expr;

interface

uses
	SysUtils,
	Classes,
	yacclib,
	lexlib,
        AST;

%}

%token <Integer> NUM
%token <String> IDENT
%type <TExpression> expr
%type <TExpression> expr2
%type <TExpression> expr3
%type <TLambda> lambda
%type <TApply> apply
%type <TLet> let
%type <TLetRec> letrec

%token LAMBDA_SYM
%token LAMBDA_ARROW_SYM
%token LET_SYM
%token LETREC_SYM
%token EQUALS_SYM
%token IN_SYM

%token ILLEGAL 		/* illegal token */

%%

input	: /* empty */
	| input '\n'		 { yyaccept; }
        | input expr '\n'	 { begin parsed := $2; yyaccept; end; }
	| error '\n'             { yyerrok; }
	;

/* Parses the LET, LET REC, FUN, and IF expressions */
expr	:  lambda		                 { $$ := $1; }
	|  let 	 				 { $$ := $1; }
      	|  letrec   	 			 { $$ := $1; }
	|  expr2				 { $$ := $1; }
 	;

/* Parses Function Application expressions */
expr2   :  expr2 expr3				 { $$ := Apply($1, $2); }
    	|  expr3 				 { $$ := $1; }

/* Parses numbers (Num), strings (Id) and expressions in parentheses */
expr3   :  NUM					{ $$ := IntegerLiteral($1); }
    	|  IDENT                        	{ $$ := Identifier($1); }
	|  '(' expr ')'         		{ $$ := $2; }

lambda  :  LAMBDA_SYM IDENT LAMBDA_ARROW_SYM expr          { $$ := Lambda($2, $4); }
	;

let     :  LET_SYM IDENT EQUALS_SYM expr IN_SYM expr 	   { $$ := Let($2, $4, $6); }
	;

letrec  :  LETREC_SYM IDENT EQUALS_SYM lambda IN_SYM expr  { $$ := LetRec($2, $4, $6); }
	;

%%

{$I exprlex.pas}

end.
