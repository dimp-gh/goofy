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
%type <TExpression> expr4
%type <TLambda> lambda
%type <TApply> apply
%type <TLet> let
%type <TLetRec> letrec
%type <TIfThenElse> ifc

%token LAMBDA_SYM
%token LAMBDA_ARROW_SYM
%token LET_SYM
%token LETREC_SYM
%token EQUALS_SYM
%token IN_SYM
%token IF_SYM
%token THEN_SYM
%token ELSE_SYM
%token UNIT_SYM
%token TRUE_SYM
%token FALSE_SYM

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
        |  ifc					 { $$ := $1; }
	|  expr2				 { $$ := $1; }
 	;

/* Parses Function Application expressions */
expr2   :  expr2 expr3				 { $$ := Apply($1, $2); }
    	|  expr3 				 { $$ := $1; }

/* Parses infix Function Application expressions */
expr3   :  expr3 '`' expr4 '`' expr4		 { $$ := Apply(Apply($3, $1), $5); }
    	|  expr4 				 { $$ := $1; }

/* Parses numbers (Num), strings (Id) and expressions in parentheses */
expr4   :  NUM					{ $$ := IntegerLiteral($1); }
    	|  IDENT                        	{ $$ := Identifier($1); }
	|  TRUE_SYM				{ $$ := BooleanLiteral(True); }
	|  FALSE_SYM				{ $$ := BooleanLiteral(False); }
	|  '(' expr ')'         		{ $$ := $2; }
	|  UNIT_SYM 				{ $$ := UnitLiteral; }

lambda  :  LAMBDA_SYM IDENT LAMBDA_ARROW_SYM expr          { $$ := Lambda($2, $4); }
	|  '(' lambda ')'                                  { $$ := $2; }
	;

let     :  LET_SYM IDENT EQUALS_SYM expr IN_SYM expr 	   { $$ := Let($2, $4, $6); }
	;

letrec  :  LETREC_SYM IDENT EQUALS_SYM lambda IN_SYM expr  { $$ := LetRec($2, $4, $6); }
	;

ifc     :  IF_SYM expr THEN_SYM expr ELSE_SYM expr 	   { $$ := IfThenElse($2, $4, $6); }
	;


%%

{$I exprlex.pas}

end.
