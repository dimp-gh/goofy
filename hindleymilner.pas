unit HindleyMilner;

{$mode objfpc}{$H+}

interface

type
   TSyntaxNode = class abstract
      function ToStr: String; virtual; abstract;
   end;

   TLambda = class(TSyntaxNode)
   private
      Variable: String;
      Body: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(v: String; b: TSyntaxNode);
   end;

   TIdent = class(TSyntaxNode)
   private
      Name: String;
   public
      function ToStr: String; override;
      constructor Create(n: String);
   end;

   TApply = class(TSyntaxNode)
   private
      Fun: TSyntaxNode;
      Argument: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(fn: TSyntaxNode; arg: TSyntaxNode);
   end;

   TLet = class(TSyntaxNode)
   private
      Variable: String;
      Definition: TSyntaxNode;
      Body: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(v: String; defn: TSyntaxNode; b: TSyntaxNode);
   end;

   TLetRec = class(TSyntaxNode)
   private
      Variable: String;
      Definition: TSyntaxNode;
      Body: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(v: String; defn: TSyntaxNode; b: TSyntaxNode);
   end;

implementation

constructor TLambda.Create(v: String; b: TSyntaxNode);
begin
   Self.Variable := v;
   Self.Body := b;
   inherited Create;
end;

function TLambda.ToStr: String;
begin
   Result := '(fn' + Self.Variable + ' => ' + Self.Body.ToStr + ')';
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

initialization

end.
