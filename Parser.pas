unit Parser;
{$mode objfpc}{$H+}
interface

uses Tokenizer, AST, SysUtils, TypInfo;

type
   EParseError = class(Exception);

function Parse(tokens: TTokenList): TExpression;

implementation

// pre-declarations of functions called from ParseExpression
function ParseExpression(tokens: TTokenList; var expr: TExpression): Boolean; forward;
function ParseIdentifier(tokens: TTokenList; var id: TIdent): Boolean; forward;
function ParseIntegerLiteral(tokens: TTokenList; var int: TIntegerLiteral): Boolean; forward;
function ParseLet(tokens: TTokenList; var let_: TLet): Boolean; forward;
function ParseLetRec(tokens: TTokenList; var letrec_: TLetRec): Boolean; forward;
function ParseLambda(tokens: TTokenList; var lamb: TLambda): Boolean; forward;
function ParseApply(tokens: TTokenList; var app: TApply): Boolean; forward;
function ParseParenExpression(tokens: TTokenList; var expr: TExpression): Boolean; forward;

function Parse(tokens: TTokenList): TExpression;
var
   res: TExpression;
begin
   if not ParseExpression(tokens, res) then
      raise EParseError.Create('Cannot parse that stuff');
   Result := res;
end;

function ParseExpression(tokens: TTokenList; var expr: TExpression): Boolean;
var
   let: TLet;
   letrec: TLetRec;
   apply: TApply;
   int: TIntegerLiteral;
   lambda: TLambda;
   id: TIdent;
   parend: TExpression;
begin
   writeln('Trying to parse expression');
   writeln('{');
   // TODO: parenthesis tokens are handled right here
   // TODO: priorities are also handled right here
   if ParseIntegerLiteral(tokens, int) then
   begin
      writeln('Parsing expression succeeded');
      writeln('Parsed expression: ', int.ToStr, '');
      writeln('}');
      Result := True;
      expr := int;
      Exit;
   end;
   if ParseIdentifier(tokens, id) then
   begin
      writeln('Parsing expression succeeded');
      writeln('Parsed expression: ', id.ToStr, '');
      writeln('}');
      Result := True;
      expr := id;
      Exit;
   end;
   if ParseLet(tokens, let) then
   begin
      writeln('Parsing expression succeeded'); 
      writeln('Parsed expression: ', let.ToStr, '');
      writeln('}');
      Result := True;
      expr := let;
      Exit;
   end;
   if ParseLetRec(tokens, letrec) then
   begin
      writeln('Parsing expression succeeded');
      writeln('Parsed expression: ', letrec.ToStr, '');
      writeln('}');
      Result := True;
      expr := letrec;
      Exit;
   end;
   if ParseLambda(tokens, lambda) then
   begin
      writeln('Parsing expression succeeded');
      writeln('Parsed expression: ', lambda.ToStr, '');
      writeln('}');
      Result := True;
      expr := lambda;
      Exit;
   end;
   if ParseParenExpression(tokens, parend) then
   begin
      writeln('Parsing expression succeeded');
      writeln('Parsed expression: ', parend.ToStr, '');
      writeln('}');
      Result := True;
      expr := parend;
      Exit;
   end;
   if ParseApply(tokens, apply) then
   begin
      writeln('Parsing expression succeeded');
      writeln('Parsed expression: ', apply.ToStr, '');
      writeln('}');
      Result := True;
      expr := apply;
      Exit;
   end;
   writeln('Parsing expression failed');
   writeln('}');
   Result := False;
   expr := nil;
end;

function ParseIdentifier(tokens: TTokenList; var id: TIdent): Boolean;
var
   tid: TToken;
begin
   writeln('Trying to parse identifier');
   writeln('{');
   tid := Pop(tokens);
   if tid.Kind <> ttIdentifier then
   begin
      writeln('Parsing identifier failed');
      writeln('}');
      Push(tid, tokens);
      Result := False;
      Exit;
   end;
   id := Ident(tid.Value);
   Result := True;
   writeln('Parsing identifier succeeded');
   writeln('Parsed identifier: ', tid.Value, '');
   writeln('}');
end;

function ParseIntegerLiteral(tokens: TTokenList; var int: TIntegerLiteral): Boolean;
var
   tint: TToken;
begin
   writeln('Trying to parse integer literal');
   writeln('{');
   tint := Pop(tokens);
   if tint.Kind <> ttIntegerLiteral then
   begin
      writeln('Parsing integer literal failed');
      writeln('}');
      Push(tint, tokens);
      Result := False;
      Exit;
   end;
   int := IntegerLiteral(tint.Value);
   Result := True;
   writeln('Parsing integer literal succeeded');
   writeln('Parsed integer: ', tint.Value, '');
   writeln('}');   
end;

function ParseLet(tokens: TTokenList; var let_: TLet): Boolean;
var
   letWord: TToken;
   name: TToken;
   eqSign: TToken;
   binding: TExpression;
   inWord: TToken;
   inExpr: TExpression;
begin
   writeln('Trying to parse let');
   writeln('{');
   // let
   letWord := Pop(tokens);
   if letWord.Kind <> ttLet then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Push(letWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('Let-word parsed');
   // <name>
   name := Pop(tokens);
   if name.Kind <> ttIdentifier then
   begin
      writeln('Parsing  let failed');
      writeln('}');
      Push(name, tokens);
      Push(letWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('Name parsed');
   // =
   eqSign := Pop(tokens);
   if eqSign.Kind <> ttEquals then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Push(eqSign, tokens);
      Push(name, tokens);
      Push(letWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('Equal sign parsed');
   // <expr>
   if not ParseExpression(tokens, binding) then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('Binding parsed');
   // in
   inWord := Pop(tokens);
   if inWord.Kind <> ttIn then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Push(inWord, tokens);
      Push(eqSign, tokens);
      Push(name, tokens);
      Push(letWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('in-word parsed');
   // <expr>
   if not ParseExpression(tokens, inExpr) then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('in-expresssion parsed');
   // filling results
   let_ := Let(name.Value, binding, inExpr);
   Result := True;
   writeln('Parsing let succeeded');
   writeln('Parsed let: ', let_.ToStr, '');
   writeln('}');
end;

function ParseLetRec(tokens: TTokenList; var letrec_: TLetRec): Boolean;
var
   letrecWord: TToken;
   name: TToken;
   eqSign: TToken;
   binding: TExpression;
   inWord: TToken;
   inExpr: TExpression;
begin
   writeln('Trying to parse letrec');
   writeln('{');
   // letrec
   letrecWord := Pop(tokens);
   if letrecWord.Kind <> ttLetRec then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Push(letrecWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('Letrec-word parsed');
   // <name>
   name := Pop(tokens);
   if name.Kind <> ttIdentifier then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Push(name, tokens);
      Push(letrecWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('let-name parsed');
   // =
   eqSign := Pop(tokens);
   if eqSign.Kind <> ttEquals then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Push(eqSign, tokens);
      Push(name, tokens);
      Push(letrecWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('equal sign parsed');
   // <expr>
   if not ParseExpression(tokens, binding) then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('Binding parsed');
   // in
   inWord := Pop(tokens);
   if inWord.Kind <> ttIn then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Push(eqSign, tokens);
      Push(name, tokens);
      Push(letrecWord, tokens);
      Push(inWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('in-word parsed');
   // <expr>
   if not ParseExpression(tokens, inExpr) then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('in-expression parsed');
   // filling results
   letrec_ := LetRec(name.Value, binding, inExpr);
   Result := True;
   writeln('Parsing letrec succeeded');
   writeln('Parsed letrec: ', letrec_.ToStr, '');
   writeln('}');
end;

function ParseLambda(tokens: TTokenList; var lamb: TLambda): Boolean;
var
   lambdaWord: TToken;
   arg: TToken;
   lambdaArrow: TToken;
   body: TExpression;
begin
   writeln('Trying to parse lambda');
   writeln('{');
   // fn
   lambdaWord := Pop(tokens);
   if lambdaWord.Kind <> ttLambda then
   begin
      writeln('Parsing lambda failed');
      writeln('}');
      Push(lambdaWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('fn-word parsed');
   // <arg>
   arg := Pop(tokens);
   if arg.Kind <> ttIdentifier then
   begin
      writeln('Parsing lambda failed');
      writeln('}');
      Push(arg, tokens);
      Push(lambdaWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('argument parsed');
   // =>
   lambdaArrow := Pop(tokens);
   if lambdaArrow.Kind <> ttLambdaArrow then
   begin
      writeln('Parsing lambda failed');
      writeln('}');
      Push(lambdaArrow, tokens);
      Push(arg, tokens);
      Push(lambdaWord, tokens);
      Result := False;
      Exit;
   end;
   writeln('lambda arrow parsed');
   // <expr>
   if not ParseExpression(tokens, body) then
   begin
      writeln('Parsing lambda failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('lambda body parsed');
   // filling results
   lamb := Lambda(arg.Value, body);
   Result := True;
   writeln('Parsing lambda succeeded');
   writeln('Parsed lambda: ', lamb.ToStr, '');
   writeln('}');
end;

function ParseApply(tokens: TTokenList; var app: TApply): Boolean;
var
   fn: TExpression;
   arg: TExpression;
begin
   writeln('Trying to parse apply');
   writeln('{');
   // <fn>
   if not ParseExpression(tokens, fn) then
   begin
      writeln('Parsing apply failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('function parsed');
   // <arg>
   if not ParseExpression(tokens, arg) then
   begin
      writeln('Parsing apply failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('argument parsed');
   // filling results
   app := Apply(fn, arg);
   Result := True;
   writeln('Parsing apply succeeded');
   writeln('Parsed apply: ', app.ToStr, '');
   writeln('}');
end;

function ParseParenExpression(tokens: TTokenList; var expr: TExpression): Boolean;
var
   openParen, closeParen: TToken;
   e: TExpression;
begin
   writeln('Trying to parse parenthesized expression');
   writeln('{');
   // (
   openParen := Pop(tokens);
   if openParen.Kind <> ttOpenParen then
   begin
      writeln('Parsing parenExpr failed');
      writeln('}');
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Open paren parsed');
   // <expr>
   if not ParseExpression(tokens, e) then
   begin
      writeln('Parsing parenExpr failed');
      writeln('}');
      Result := False;
      Exit;
   end;
   writeln('subexpression parsed');
   // )
   closeParen := Pop(tokens);
   if closeParen.Kind <> ttCloseParen then
   begin
      writeln('Parsing parenExpr failed');
      writeln('}');
      Push(closeParen, tokens);
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Close paren parsed');
   // filling results
   expr := e;
   Result := True;
   writeln('Parsing parenExpr succeeded');
   writeln('Parsed paren expr: (', expr.ToStr, ')');
   writeln('}');
end;

initialization
{
;; bad grammar
E ::= Id
E ::= Int
E ::= fn Id => E
E ::= E E
E ::= let Id = E in E
E ::= ( E )

;; better grammar (without left recursion)
E :: Int
E ::= Id E'
E ::= fn Id => E E'
E ::= let Id = E in E E'
E ::= ( E ) E'
E' ::= E E' | Nothing
}
   
end.
