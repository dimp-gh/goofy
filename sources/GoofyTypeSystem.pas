unit GoofyTypeSystem;
{$mode objfpc}{$H+}
interface
uses AST, HMTypes, HMDataStructures, HindleyMilner, Builtins;

type
   TGoofyTypeSystem = class(THMTypeSystem)
   private
      Env: TTypeEnvironment;
   public
      constructor Create(bs: TGoofyBuiltins);
      function GetExprType(ast: TExpression): TType;
      function GetExprType(ast: TExpression; environ: TTypeEnvironment): TType;
      function GetExprTypeStr(ast: TExpression): String;
   end;

implementation

constructor TGoofyTypeSystem.Create(bs: TGoofyBuiltins);
begin
   inherited Create;
   Self.Env := bs.GetBuiltinTypes;
   //Self.PrintEnvironment(Self.Env);
end;

function TGoofyTypeSystem.GetExprType(ast: TExpression): TType;
begin
   Result := Self.Analyse(ast, Self.Env);
end;

function TGoofyTypeSystem.GetExprType(ast: TExpression; environ: TTypeEnvironment): TType;
begin
   Result := Self.Analyse(ast, environ);
end;

function TGoofyTypeSystem.GetExprTypeStr(ast: TExpression): String;
begin
   try
      Result := Self.GetExprType(ast).ToStr;
   except
      on e:EParseError do Result := 'Parse error: ' + e.Message;
      on e:ETypeError do Result := 'Type error: ' + e.Message;
   end;      
end;

initialization

end.
