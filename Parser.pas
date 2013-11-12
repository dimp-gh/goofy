unit Parser;
{$mode objfpc}{$H+}
interface

{
Current CFG: (non-terminals are starting with capital letters, others are terminals)
Expr ::= Identifier
Expr ::= Int
Expr ::= (fn Identifier => Expr)
Expr ::= (Expr Expr)
Expr ::= (let Identifier = Expr in Expr)
Expr ::= (letrec Identifier = Expr in Expr)
}

uses Tokenizer, AST, SysUtils, TypInfo;

type
   EParseError = class(Exception);

function Parse(tokens: TTokenList): TExpression;

implementation

// pre-declarations of functions called from ParseExpression
function ParseExpression(tokens: TTokenList; var expr: TExpression): Boolean; forward;
function ParseIdentifier(tokens: TTokenList; var id: TIdentifier): Boolean; forward;
function ParseIntegerLiteral(tokens: TTokenList; var int: TIntegerLiteral): Boolean; forward;
function ParseLet(tokens: TTokenList; var let_: TLet): Boolean; forward;
function ParseLetRec(tokens: TTokenList; var letrec_: TLetRec): Boolean; forward;
function ParseLambda(tokens: TTokenList; var lamb: TLambda): Boolean; forward;
function ParseApply(tokens: TTokenList; var app: TApply): Boolean; forward;

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
   id: TIdentifier;
begin
   writeln('Trying to parse expression');
   writeln('{');
   writeln('Token list:');
   PrintTokenList(tokens);
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

function ParseIdentifier(tokens: TTokenList; var id: TIdentifier): Boolean;
var
   tid: TToken;
begin
   writeln('Trying to parse identifier');
   writeln('{');
   writeln('Token list:');
   PrintTokenList(tokens);
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
   writeln('Token list:');
   PrintTokenList(tokens);
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
   openParen, closeParen: TToken;
begin
   writeln('Trying to parse let');
   writeln('{');
   writeln('Token list:');
   PrintTokenList(tokens);
   // (
   openParen := Pop(tokens);
   if openParen.Kind <> ttOpenParen then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Open paren parsed');
   // let
   letWord := Pop(tokens);
   if letWord.Kind <> ttLet then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Push(letWord, tokens);
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
   // )
   closeParen := Pop(tokens);
   if closeParen.Kind <> ttCloseParen then
   begin
      writeln('Parsing let failed');
      writeln('}');
      Push(closeParen, tokens);
      Push(inWord, tokens);
      Push(eqSign, tokens);
      Push(name, tokens);
      Push(letWord, tokens);
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Close paren parsed');
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
   openParen, closeParen: TToken;
begin
   writeln('Trying to parse letrec');
   writeln('{');
   writeln('Token list:');
   PrintTokenList(tokens);
   // (
   openParen := Pop(tokens);
   if openParen.Kind <> ttOpenParen then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Open paren parsed');
   // letrec
   letrecWord := Pop(tokens);
   if letrecWord.Kind <> ttLetRec then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Push(letrecWord, tokens);
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
   // )
   closeParen := Pop(tokens);
   if closeParen.Kind <> ttCloseParen then
   begin
      writeln('Parsing letrec failed');
      writeln('}');
      Push(closeParen, tokens);
      Push(eqSign, tokens);
      Push(name, tokens);
      Push(letrecWord, tokens);
      Push(inWord, tokens);
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Close paren parsed');
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
   openParen, closeParen: TToken;
begin
   writeln('Trying to parse lambda');
   writeln('{');
   writeln('Token list:');
   PrintTokenList(tokens);
   // (
   openParen := Pop(tokens);
   if openParen.Kind <> ttOpenParen then
   begin
      writeln('Parsing lambda failed');
      writeln('}');
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Open paren parsed');
   // fn
   lambdaWord := Pop(tokens);
   if lambdaWord.Kind <> ttLambda then
   begin
      writeln('Parsing lambda failed');
      writeln('}');
      Push(lambdaWord, tokens);
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
      Push(openParen, tokens);
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
   // )
   closeParen := Pop(tokens);
   if closeParen.Kind <> ttCloseParen then
   begin
      writeln('Parsing lambda failed');
      writeln('}');
      Push(closeParen, tokens);
      Push(lambdaArrow, tokens);
      Push(arg, tokens);
      Push(lambdaWord, tokens);
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Close paren parsed');
   // filling results
   lamb := Lambda(arg.Value, body);
   Result := True;
   writeln('Parsing lambda succeeded');
   writeln('Parsed lambda: ', lamb.ToStr, '');
   writeln('}');
end;

function ParseApply(tokens: TTokenList; var app: TApply): Boolean;
var
   openParen, closeParen: TToken;
   fn: TExpression;
   arg: TExpression;
begin
   writeln('Trying to parse apply');
   writeln('{');
   writeln('Token list:');
   PrintTokenList(tokens);
   // (
   openParen := Pop(tokens);
   if openParen.Kind <> ttOpenParen then
   begin
      writeln('Parsing apply failed');
      writeln('}');
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Open paren parsed');
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
   // )
   closeParen := Pop(tokens);
   if closeParen.Kind <> ttCloseParen then
   begin
      writeln('Parsing apply failed');
      writeln('}');
      Push(closeParen, tokens);
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   writeln('Close paren parsed');
   // filling results
   app := Apply(fn, arg);
   Result := True;
   writeln('Parsing apply succeeded');
   writeln('Parsed apply: ', app.ToStr, '');
   writeln('}');
end;


initialization
   
end.
