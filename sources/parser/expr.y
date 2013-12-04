%{unit expr;

interface

uses
	SysUtils,
	Classes,
	yacclib,
	lexlib,
        AST,
	HMTypes,
	ParserHelper;

%}

%token <Int64> NUM
%token <String> IDENT
%token <String> TYPEIDENT
%token <String> STRINGLIT
%type <TExpression> expression
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
%type <TStatementList> statements
%type <TModule> module
%type <String> module_header
%type <TDoExpression> do_expression
%type <TStatementList> do_inside
%type <TStatement> do_subj
%type <String> type_id
%type <TTypeDeclaration> type_decl
%type <TTypeList> union_type
%type <TType> type_expr
%type <TTypeList> type_components
%type <TType> type_comp
%type <TTypeVariable> type_var
%type <TTypeVariableList> type_vars

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
%token MODULE_SYM
%token WHERE_SYM
%token DO_SYM
%token DATA_SYM

%token ILLEGAL 		/* illegal token */

%%

input	: /* empty */ /* NOTE: why match empty input? */
        | input expression     	 { begin parsed := $2; yyaccept; end; }
	| input statement	 { begin parsed := $2; yyaccept; end; }
	| input module     	 { begin parsed := $2; yyaccept; end; }
	| error                  { yyerrok; }
	;

statement  :  function_declaration		 { $$ := $1; }
	   |  value_assignment			 { $$ := $1; }
	   |  type_decl				 { $$ := $1; }
	   ;

value_assignment : VAL_SYM IDENT EQUALS_SYM expression { $$ := ValueDecl($2, $4); }
		 ;

statements  :  statement statements	         { $$ := PrependStmt($1, $2); }
     	    |  statement 			 { $$ := SingleStmt($1); }
     	    ;

module  :  module_header statements		 { $$ := Module($1, $2); }
	;

module_header  :  MODULE_SYM TYPEIDENT WHERE_SYM	 { $$ := $2; }	 
	       ;

/* Parses the LET, LETREC, FN, IF, CASE expressions */
expression	:  lambda		                 { $$ := $1; }
		|  let 	 				 { $$ := $1; }
		|  letrec   	 			 { $$ := $1; }
        	|  ifc					 { $$ := $1; }
		|  casec				 { $$ := $1; }
		|  do_expression			 { $$ := $1; }                     
		|  expr2				 { $$ := $1; }
 		;

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
	|  '(' expression ',' expression ')'    { $$ := PairLiteral($2, $4); }
	|  '(' expression ')'         		{ $$ := $2; }
	|  UNIT_SYM 				{ $$ := UnitLiteral; }
	|  STRINGLIT				{ $$ := StringLiteral(Unescape(CutStringLiteral($1))); }

lambda  :  LAMBDA_SYM IDENT LAMBDA_ARROW_SYM expression    { $$ := Lambda($2, $4); }
	|  '(' lambda ')'                                  { $$ := $2; }
	;

let     :  LET_SYM IDENT EQUALS_SYM expression IN_SYM expression { $$ := Let($2, $4, $6); }
	;

letrec  :  LETREC_SYM IDENT EQUALS_SYM lambda IN_SYM expression  { $$ := LetRec($2, $4, $6); }
	;

ifc     :  IF_SYM expression THEN_SYM expression ELSE_SYM expression  { $$ := IfThenElse($2, $4, $6); }
	;

casec   :  CASE_SYM expression OF_SYM clauses END_SYM 	   { $$ := CaseOf($2, $4); }
	;

clauses :  clause ';' clauses				   { $$ := PrependClause($1, $3); }
	|  clause 					   { $$ := SingleClause($1); }
	;

clause  :  expr4 CASE_ARROW_SYM expression		   { $$ := Clause($1, $3) }
	;

function_declaration  :  FUN_SYM fun_clauses               { $$ := $2; }
 		      ;

fun_clauses :  IDENT fun_clause '|' fun_clauses		   { $$ := FunctionDecl($1, PrependClause($2, GetClauses($4))); }
	    |  IDENT fun_clause 			   { $$ := FunctionDecl($1, SingleClause($2)); }
	    ;

fun_clause  :  expr4 EQUALS_SYM expression		   { $$ := Clause($1, $3); }
	    ;

do_expression  :  DO_SYM '{' do_inside expression '}'	   { $$ := DoExpression($3, $4); }
	       ;

do_inside   :  do_subj do_inside			   { $$ := PrependStmt($1, $2); }
     	    |  do_subj  				   { $$ := SingleStmt($1); }
     	    ;

do_subj     :  statement ','				   { $$ := $1; }
	    |  DO_SYM expression ',' 			   { $$ := ValueDecl('_', $2); }
	    ;

type_id     :  TYPEIDENT				   { $$ := $1; }
	    ;

type_decl : DATA_SYM type_id type_vars EQUALS_SYM union_type  { $$ := TypeDecl($2, $3, $5); }
	  ;

union_type  :  type_expr '|' union_type			   { $$ := PrependType($1, $3); }
	    |  type_expr 				   { $$ := SingleType($1); }
	    ;

type_expr   :  type_id type_components			   { $$ := TParameterizedType.Create($1, $2); }
	    |  type_id					   { $$ := CreateType($1); }
	    ;

type_components  : type_comp type_components		   { $$ := PrependType($1, $2); }
		 | type_comp				   { $$ := SingleType($1); }
		 ;

type_comp   :  type_var					   { $$ := $1; }
	    |  '(' type_expr ')'			   { $$ := $2; }
	    ;

type_var  :  IDENT 					   { $$ := TTypeVariable.Create($1); }
	  ;

type_vars :  type_var type_vars				   { $$ := PrependTypeVar($1 ,$2); }
	  |  type_var 					   { $$ := SingleTypeVar($1); }
	  |  						   { $$ := NoTypeVars; }
	  ;

%%

{$I exprlex.pas}

end.
