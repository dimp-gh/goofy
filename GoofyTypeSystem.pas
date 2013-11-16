unit GoofyTypeSystem;
{$mode objfpc}{$H+}
interface
uses AST, HMTypes, HMDataStructures, HindleyMilner, Builtins;

type
   TGoofyTypeSystem = class(THMTypeSystem)
   private
      Env: TTypeEnvironment;
   public
      constructor Create;
      function GetExprType(ast: TExpression): TType;
      function GetExprTypeStr(ast: TExpression): String;
   end;

implementation

constructor TGoofyTypeSystem.Create;
var
   v1, v2, v3: TTypeVariable;
begin
   inherited Create;
   Self.Env := EnvNew;
   
   v1 := Self.VarGen.GenerateVariable;
   v2 := Self.VarGen.GenerateVariable;
   v3 := Self.VarGen.GenerateVariable;
   
   Self.Env := EnvInsert(Self.Env, 'pair', CreateFunType(v1, CreateFunType(v2, CreatePairType(v1, v2))));
   Self.Env := EnvInsert(Self.Env, 'true', Self.Bool);
   Self.Env := EnvInsert(Self.Env, 'cond', CreateFunType(Self.Bool, CreateFunType(v3, CreateFunType(v3, v3))));
   Self.Env := EnvInsert(Self.Env, 'zero', CreateFunType(Self.Int, Self.Bool));
   Self.Env := EnvInsert(Self.Env, 'pred', CreateFunType(Self.Int, Self.Int));
   Self.Env := EnvInsert(Self.Env, 'times', CreateFunType(Self.Int, CreateFunType(Self.Int, Self.Int)));

   Self.Env := EnvInsert(Self.Env, 'forty-two', Self.Int);
   
   //Self.PrintEnvironment(Self.Env);
end;

function TGoofyTypeSystem.GetExprType(ast: TExpression): TType;
begin
   Result := Self.Analyse(ast, Self.Env);
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
