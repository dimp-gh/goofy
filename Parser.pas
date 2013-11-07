unit Parser;
{$mode objfpc}{$H+}
interface

uses Tokenizer, AST, SysUtils, TypInfo;

type
   EParseError = class(Exception);

function Parse(var tokens: TTokenList): TExpression;

implementation

// pre-declarations of functions called from ParseExpression
function ParseExpression(var tokens: TTokenList; var expr: TExpression): Boolean; forward;
function ParseIdentifier(var tokens: TTokenList; var id: TIdent): Boolean; forward;
function ParseIntegerLiteral(var tokens: TTokenList; var int: TIntegerLiteral): Boolean; forward;
function ParseLet(var tokens: TTokenList; var let_: TLet): Boolean; forward;
function ParseLetRec(var tokens: TTokenList; var letrec_: TLetRec): Boolean; forward;
function ParseLambda(var tokens: TTokenList; var lamb: TLambda): Boolean; forward;
function ParseApply(var tokens: TTokenList; var app: TApply): Boolean; forward;
function ParseParenExpression(var tokens: TTokenList; var expr: TExpression): Boolean; forward;

function Parse(var tokens: TTokenList): TExpression;
var
   res: TExpression;
begin
   if not ParseExpression(tokens, res) then
      raise EParseError.Create('Cannot parse that stuff');
   Result := res;
end;

function ParseExpression(var tokens: TTokenList; var expr: TExpression): Boolean;
var
   let: TLet;
   letrec: TLetRec;
   apply: TApply;
   int: TIntegerLiteral;
   lambda: TLambda;
   id: TIdent;
   parend: TExpression;
begin
   // TODO: parenthesis tokens are handled right here
   // TODO: priorities are also handled right here
   if ParseIntegerLiteral(tokens, int) then
   begin
      Result := True;
      expr := int;
   end
   else if ParseLet(tokens, let) then
   begin
      Result := True;
      expr := let;
   end
   else if ParseLetRec(tokens, letrec) then
   begin
      Result := True;
      expr := letrec
   end
   else if ParseLambda(tokens, lambda) then
   begin
      Result := True;
      expr := lambda;
   end
   else if ParseApply(tokens, apply) then
   begin
      Result := True;
      expr := apply;
   end
   else if ParseIdentifier(tokens, id) then
   begin
      Result := True;
      expr := id;
   end
   else if ParseParenExpression(tokens, parend) then
   begin
      Result := True;
      expr := parend;
   end
   else
   begin
      Result := False;
      expr := nil;
   end;
end;

function ParseIdentifier(var tokens: TTokenList; var id: TIdent): Boolean;
var
   tid: TToken;
begin
   tid := Pop(tokens);
   if tid.Kind <> ttIdentifier then
   begin
      Push(tid, tokens);
      Result := False;
      Exit;
   end;
   id := Ident(tid.Value);
   Result := True;
end;

function ParseIntegerLiteral(var tokens: TTokenList; var int: TIntegerLiteral): Boolean;
var
   tint: TToken;
begin
   tint := Pop(tokens);
   if tint.Kind <> ttIntegerLiteral then
   begin
      Push(tint, tokens);
      Result := False;
      Exit;
   end;
   int := IntegerLiteral(tint.Value);
   Result := True;
end;

function ParseLet(var tokens: TTokenList; var let_: TLet): Boolean;
var
   letWord: TToken;
   name: TToken;
   eqSign: TToken;
   binding: TExpression;
   inWord: TToken;
   inExpr: TExpression;
begin
   // let
   letWord := Pop(tokens);
   if letWord.Kind <> ttLet then
   begin
      Push(letWord, tokens);
      Result := False;
      Exit;
   end;
   // <name>
   name := Pop(tokens);
   if name.Kind <> ttIdentifier then
   begin
      Push(name, tokens);
      Result := False;
      Exit;
   end;
   // =
   eqSign := Pop(tokens);
   if eqSign.Kind <> ttEquals then
   begin
      Push(eqSign, tokens);
      Result := False;
      Exit;
   end;
   // <expr>
   if not ParseExpression(tokens, binding) then
   begin
      Result := False;
      Exit;
   end;
   // in
   inWord := Pop(tokens);
   if inWord.Kind <> ttIn then
   begin
      Push(inWord, tokens);
      Result := False;
      Exit;
   end;
   // <expr>
   if not ParseExpression(tokens, inExpr) then
   begin
      Result := False;
      Exit;
   end;
   // filling results
   let_ := Let(name.Value, binding, inExpr);
   Result := True;
end;

function ParseLetRec(var tokens: TTokenList; var letrec_: TLetRec): Boolean;
var
   letrecWord: TToken;
   name: TToken;
   eqSign: TToken;
   binding: TExpression;
   inWord: TToken;
   inExpr: TExpression;
begin
   // letrec
   letrecWord := Pop(tokens);
   if letrecWord.Kind <> ttLetRec then
   begin
      Push(letrecWord, tokens);
      Result := False;
      Exit;
   end;
   // <name>
   name := Pop(tokens);
   if name.Kind <> ttIdentifier then
   begin
      Push(name, tokens);
      Result := False;
      Exit;
   end;
   // =
   eqSign := Pop(tokens);
   if eqSign.Kind <> ttEquals then
   begin
      Push(eqSign, tokens);
      Result := False;
      Exit;
   end;
   // <expr>
   if not ParseExpression(tokens, binding) then
   begin
      Result := False;
      Exit;
   end;
   // in
   inWord := Pop(tokens);
   if inWord.Kind <> ttIn then
   begin
      Push(inWord, tokens);
      Result := False;
      Exit;
   end;
   // <expr>
   if not ParseExpression(tokens, inExpr) then
   begin
      Result := False;
      Exit;
   end;
   // filling results
   letrec_ := LetRec(name.Value, binding, inExpr);
   Result := True;
end;

function ParseLambda(var tokens: TTokenList; var lamb: TLambda): Boolean;
var
   lambdaWord: TToken;
   arg: TToken;
   lambdaArrow: TToken;
   body: TExpression;
begin
   // fn
   lambdaWord := Pop(tokens);
   if lambdaWord.Kind <> ttLambda then
   begin
      Push(lambdaWord, tokens);
      Result := False;
      Exit;
   end;
   // <arg>
   arg := Pop(tokens);
   if arg.Kind <> ttIdentifier then
   begin
      Push(arg, tokens);
      Result := False;
      Exit;
   end;
   // =>
   lambdaArrow := Pop(tokens);
   if lambdaArrow.Kind <> ttLambdaArrow then
   begin
      Push(lambdaArrow, tokens);
      Result := False;
      Exit;
   end;
   // <expr>
   if not ParseExpression(tokens, body) then
   begin
      Result := False;
      Exit;
   end;
   // filling results
   lamb := Lambda(arg.Value, body);
   Result := True;
end;

function ParseApply(var tokens: TTokenList; var app: TApply): Boolean;
var
   fn: TExpression;
   arg: TExpression;
begin
   // <fn>
   if not ParseExpression(tokens, fn) then
   begin
      Result := False;
      Exit;
   end;
   // <arg>
   if not ParseExpression(tokens, arg) then
   begin
      Result := False;
      Exit;
   end;
   // filling results
   app := Apply(fn, arg);
   Result := True;
end;

function ParseParenExpression(var tokens: TTokenList; var expr: TExpression): Boolean;
var
   openParen, closeParen: TToken;
   e: TExpression;
begin
   writeln('DEBUG: hey there, officer');
   // (
   writeln('DEBUG: checking for open paren');
   openParen := Pop(tokens);
   writeln('DEBUG: My token is ''', openParen.Value, '''');
   if openParen.Kind <> ttOpenParen then
   begin
      writeln('DEBUG: nah, it''s not open paren');
      Push(openParen, tokens);
      Result := False;
      Exit;
   end;
   // <expr>
   if not ParseExpression(tokens, e) then
   begin
      Result := False;
      Exit;
   end;
   // )
   closeParen := Pop(tokens);
   if closeParen.Kind <> ttCloseParen then
   begin
      Push(closeParen, tokens);
      Result := False;
      Exit;
   end;
   // filling results
   expr := e;
   Result := True;   
end;

{ parsing routines checklist
  * [ ] ParseExpression (with tricky parentheses case and priorities)
  * [x] [ ] ParseIdentifier
  * [x] [ ] ParseIntegerLiteral
  * [x] [x] ParseLambda
  * [x] [ ] ParseApply
  * [x] [x] ParseLet
  * [x] [x] ParseLetRec
}
initialization
   
end.
