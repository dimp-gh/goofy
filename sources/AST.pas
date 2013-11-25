unit AST;
{$mode objfpc}{$H+}
interface

uses
   Sysutils;

type
   TExpression = class abstract
      function ToStr: String; virtual; abstract;
   end;

   TIdentifier = class(TExpression)
   public
      Name: String;
      function ToStr: String; override;
      constructor Create(n: String);
   end;
   
   TIntegerLiteral = class(TExpression)
   public
      Value: Integer;
      function ToStr: String; override;
      constructor Create(v: String);
   end;
   
   TLambda = class(TExpression)
   public
      Variable: String;
      Body: TExpression;
      function ToStr: String; override;
      constructor Create(v: String; b: TExpression);
   end;

   TApply = class(TExpression)
   public
      Fun: TExpression;
      Argument: TExpression;
      function ToStr: String; override;
      constructor Create(fn: TExpression; arg: TExpression);
   end;

   TLet = class(TExpression)
   public
      Variable: String;
      Definition: TExpression;
      Body: TExpression;
      function ToStr: String; override;
      constructor Create(v: String; defn: TExpression; b: TExpression);
   end;

   TLetRec = class(TExpression)
   public
      Variable: String;
      Definition: TLambda;
      Body: TExpression;
      function ToStr: String; override;
      constructor Create(v: String; defn: TLambda; b: TExpression);
   end;

function Identifier(n: String): TIdentifier;
function IntegerLiteral(v: Integer): TIntegerLiteral;
function IntegerLiteral(v: String): TIntegerLiteral;
function Lambda(v: String; b: TExpression): TLambda;
function Apply(fn: TExpression; arg: TExpression): TApply;
function Let(v: String; defn: TExpression; b: TExpression): TLet;
function LetRec(v: String; defn: TLambda; b: TExpression): TLetRec;

implementation

constructor TIntegerLiteral.Create(v: String);
begin
   Self.Value := StrToInt(v);
   inherited Create;
end;

function TIntegerLiteral.ToStr: String;
begin
   Result := IntToStr(Self.Value);
end;

constructor TIdentifier.Create(n: String);
begin
   Self.Name := n;
   inherited Create;
end;

function TIdentifier.ToStr: String;
begin
   Result := Self.Name;
end;

constructor TLambda.Create(v: String; b: TExpression);
begin
   Self.Variable := v;
   Self.Body := b;
   inherited Create;
end;

function TLambda.ToStr: String;
begin
   Result := '(fn ' + Self.Variable + ' => ' + Self.Body.ToStr + ')';
end;

constructor TApply.Create(fn, arg: TExpression);
begin
   Self.Fun := fn;
   Self.Argument := arg;
   inherited Create;
end;

function TApply.ToStr: String;
begin
   Result := '(' + Self.Fun.ToStr + ' ' + Self.Argument.ToStr + ')';
end;

constructor TLet.Create(v: String; defn, b: TExpression);
begin
   Self.Variable := v;
   Self.Definition := defn;
   Self.Body := b;
   inherited Create;
end;

function TLet.ToStr: String;
begin
   Result := '(' + 'let ' + Self.Variable + ' = ' + Self.Definition.ToStr + ' in ' + Self.Body.ToStr + ')';
end;

constructor TLetRec.Create(v: String; defn: TLambda; b: TExpression);
begin
   Self.Variable := v;
   Self.Definition := defn;
   Self.Body := b;
   inherited Create;
end;

function TLetRec.ToStr: String;
begin
   Result := '(' + 'letrec ' + Self.Variable + ' = ' + Self.Definition.ToStr + ' in ' + Self.Body.ToStr + ')';
end;

// A few convenient functions for creating AST
function Identifier(n: String): TIdentifier;
begin
   Result := TIdentifier.Create(n);
end;

function IntegerLiteral(v: Integer): TIntegerLiteral;
begin
   Result := TIntegerLiteral.Create(IntToStr(v));
end;

function IntegerLiteral(v: String): TIntegerLiteral;
begin
   Result := TIntegerLiteral.Create(v);
end;

function Lambda(v: String; b: TExpression): TLambda;
begin
   Result := TLambda.Create(v, b);
end;

function Apply(fn: TExpression; arg: TExpression): TApply;
begin
   Result := TApply.Create(fn, arg);
end;

function Let(v: String; defn: TExpression; b: TExpression): TLet;
begin
   Result := TLet.Create(v, defn, b);
end;

function LetRec(v: String; defn: TLambda; b: TExpression): TLetRec;
begin
   Result := TLetRec.Create(v, defn, b);
end;


initialization

end.
