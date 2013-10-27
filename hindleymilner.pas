unit HindleyMilner;

{$mode objfpc}{$H+}

interface

type
   ISyntaxNode = interface
      function ToString: String;
   end;

   TLambda = class(TInterfacedObject, ISyntaxNode)
   private
      Variable: String;
      Body: ISyntaxNode;
   public
      function ToStr: String;
      constructor Create(v: String; b: ISyntaxNode);
   end;

   TIdent = class(TInterfacedObject, ISyntaxNode)
      function ToStr: String;
   end;

   TApply = class(TInterfacedObject, ISyntaxNode)
      function ToStr: String;
   end;

   TLet = class(TInterfacedObject, ISyntaxNode)
      function ToStr: String;
   end;

   TLetRec = class(TInterfacedObject, ISyntaxNode)
      function ToStr: String;
   end;

implementation

constructor TLambda.Create(v: String; b: ISyntaxNode);
begin
   Self.Variable := v;
   Self.Body := b;
   inherited Create;
end;

function TLambda.ToStr: String;
var BodyStr: String;
begin
   //BodyStr := Self.Body.ToStr;
   Result := 'fn' + Self.Variable + ' => ' + '<unknown>';
end;

function TIdent.ToStr: String;
begin
   Result := 'ident';
end;

function TApply.ToStr: String;
begin
   Result := 'apply';
end;

function TLet.ToStr: String;
begin
   Result := 'let';
end;

function TLetRec.ToStr: String;
begin
   Result := 'letrec';
end;

initialization

end.
