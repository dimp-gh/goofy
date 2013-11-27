%{unit expr;

interface

uses
	SysUtils,
	Classes,
	yacclib,
	lexlib,
        AST,
	ParserHelper;

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
%type <TCaseOf> casec
%type <TClause> clause
%type <TClauseList> clauses
%type <TValueDeclaration> function_declaration
%type <TValueDeclaration> fun_clauses
%type <TClause> fun_clause
%type <TStatement> statement
%type <TValueDeclaration> value_assignment

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
%token CASE_SYM
%token OF_SYM
%token CASE_ARROW_SYM
%token END_SYM
%token FUN_SYM
%token VAL_SYM

%token ILLEGAL 		/* illegal token */

%%

input	: /* empty */
	| input '\n'		 { yyaccept; }
        | input expr '\n'	 { begin parsed := $2; yyaccept; end; }
	| input statement '\n'   { begin parsed := $2; yyaccept; end; }
/*	| input module '\n'	 { begin parsed := $2; yyaccept; end; }*/
	| error '\n'             { yyerrok; }
	;

/* Parses the LET, LETREC, FN, IF, CASE expressions */
expr	:  lambda		                 { $$ := $1; }
	|  let 	 				 { $$ := $1; }
      	|  letrec   	 			 { $$ := $1; }
        |  ifc					 { $$ := $1; }
	|  casec				 { $$ := $1; }
	|  expr2				 { $$ := $1; }
 	;

statement  :  function_declaration		 { $$ := $1; }
	   |  value_assignment			 { $$ := $1; }
	   ;

value_assignment : VAL_SYM IDENT EQUALS_SYM expr { $$ := ValueDecl($2, $4); }
		 ;

/*
statements  :  statement statements		 { $$ := PrependStmt($1, $2); }
     	    |  statement 			 { $$ := SingleStmt($1, $2); }
     	    ;

module  :  statements				 { $$ := Module($1); }
*/

/* Parses Function Application expressions */
expr2   :  expr2 expr3				 { $$ := Apply($1, $2); }
    	|  expr3 				 { $$ := $1; }

/* Parses infix Function Application expressions */
expr3   :  expr3 '`' expr4 '`' expr4		 { $$ := Apply(Apply($3, $1), $5); }
    	|  expr4 				 { $$ := $1; }

/* Parses literals, identifiers and expressions in parentheses */
expr4   :  NUM					{ $$ := IntegerLiteral($1); }
    	|  IDENT                        	{ $$ := Identifier($1); }
	|  TRUE_SYM				{ $$ := BooleanLiteral(True); }
	|  FALSE_SYM				{ $$ := BooleanLiteral(False); }
	|  '(' expr ',' expr ')'		{ $$ := PairLiteral($2, $4); }
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

casec   :  CASE_SYM expr OF_SYM clauses END_SYM 	   { $$ := CaseOf($2, $4); }
	;

clauses :  clause ';' clauses				   { $$ := PrependClause($1, $3); }
	|  clause 					   { $$ := SingleClause($1); }
	;

clause  :  expr4 CASE_ARROW_SYM expr			   { $$ := Clause($1, $3) }
	;

function_declaration  :  FUN_SYM fun_clauses               { $$ := $2; }
 		      ;

fun_clauses :  IDENT fun_clause '|' fun_clauses		   { $$ := FunctionDecl($1, PrependClause($2, GetClauses($4))); }
	    |  IDENT fun_clause 			   { $$ := FunctionDecl($1, SingleClause($2)); }
	    ;

fun_clause  :  expr4 EQUALS_SYM expr			   { $$ := Clause($1, $3); }
	    ;


%%

{$I exprlex.pas}

end.
