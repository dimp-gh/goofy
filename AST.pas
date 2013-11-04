unit AST;
{$mode objfpc}{$H+}

interface

type
   TSyntaxNode = class abstract
      function ToStr: String; virtual; abstract;
   end;

   TIdent = class(TSyntaxNode)
   public
      Name: String;
      function ToStr: String; override;
      constructor Create(n: String);
   end;

   TLambda = class(TSyntaxNode)
   public
      Variable: String;
      Body: TSyntaxNode;
      function ToStr: String; override;
      constructor Create(v: String; b: TSyntaxNode);
   end;

   TApply = class(TSyntaxNode)
   public
      Fun: TSyntaxNode;
      Argument: TSyntaxNode;
      function ToStr: String; override;
      constructor Create(fn: TSyntaxNode; arg: TSyntaxNode);
   end;

   TLet = class(TSyntaxNode)
   public
      Variable: String;
      Definition: TSyntaxNode;
      Body: TSyntaxNode;
      function ToStr: String; override;
      constructor Create(v: String; defn: TSyntaxNode; b: TSyntaxNode);
   end;

   TLetRec = class(TSyntaxNode)
   public
      Variable: String;
      Definition: TSyntaxNode;
      Body: TSyntaxNode;
      function ToStr: String; override;
      constructor Create(v: String; defn: TSyntaxNode; b: TSyntaxNode);
   end;

function Ident(n: String): TIdent;
function Lambda(v: String; b: TSyntaxNode): TLambda;
function Apply(fn: TSyntaxNode; arg: TSyntaxNode): TApply;
function Let(v: String; defn: TSyntaxNode; b: TSyntaxNode): TLet;
function LetRec(v: String; defn: TSyntaxNode; b: TSyntaxNode): TLetRec;

implementation

constructor TLambda.Create(v: String; b: TSyntaxNode);
begin
   Self.Variable := v;
   Self.Body := b;
   inherited Create;
end;

function TLambda.ToStr: String;
begin
   Result := '(fn ' + Self.Variable + ' => ' + Self.Body.ToStr + ')';
end;

constructor TIdent.Create(n: String);
begin
   Self.Name := n;
   inherited Create;
end;

function TIdent.ToStr: String;
begin
   Result := Self.Name;
end;

constructor TApply.Create(fn, arg: TSyntaxNode);
begin
   Self.Fun := fn;
   Self.Argument := arg;
   inherited Create;
end;

function TApply.ToStr: String;
begin
   Result := '(' + Self.Fun.ToStr + ' ' + Self.Argument.ToStr + ')';
end;

constructor TLet.Create(v: String; defn, b: TSyntaxNode);
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

constructor TLetRec.Create(v: String; defn, b: TSyntaxNode);
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
function Ident(n: String): TIdent;
begin
   Result := TIdent.Create(n);
end;

function Lambda(v: String; b: TSyntaxNode): TLambda;
begin
   Result := TLambda.Create(v, b);
end;

function Apply(fn: TSyntaxNode; arg: TSyntaxNode): TApply;
begin
   Result := TApply.Create(fn, arg);
end;

function Let(v: String; defn: TSyntaxNode; b: TSyntaxNode): TLet;
begin
   Result := TLet.Create(v, defn, b);
end;

function LetRec(v: String; defn: TSyntaxNode; b: TSyntaxNode): TLetRec;
begin
   Result := TLetRec.Create(v, defn, b);
end;


initialization

end.
