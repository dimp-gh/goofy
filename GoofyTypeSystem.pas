unit GoofyTypeSystem;
{$mode objfpc}{$H+}
interface
uses AST, HMTypes, HMDataStructures, HindleyMilner;

type
   TGoofyTypeSystem = class(TTypeSystem)
   private
      Env: TEnvironment;
   public
      constructor Create;
      function GetExprType(ast: TSyntaxNode): TType;
      function GetExprTypeStr(ast: TSyntaxNode): String;
   end;

implementation

constructor TGoofyTypeSystem.Create;
var
   v1, v2, v3: TVariable;
   args: array of TType;
   pairType: TOper;
begin
   inherited Create;
   Self.Env := EnvNew;
   
   v1 := Self.GenerateVariable;
   v2 := Self.GenerateVariable;
   SetLength(args, 2);
   args[0] := v1;
   args[1] := v2;
   pairType := TOper.Create('*', args);
   
   v3 := Self.GenerateVariable;
   
   // "pair" -> TypeSystem.Function(var1, TypeSystem.Function(var2, pairtype)),
   Self.Env := EnvInsert(Self.Env, 'pair', CreateFunType(v1, CreateFunType(v2, pairType)));
   // "true" -> TypeSystem.Bool,
   Self.Env := EnvInsert(Self.Env, 'true', Self.Bool);
   // "cond" -> TypeSystem.Function(TypeSystem.Bool, TypeSystem.Function(var3, TypeSystem.Function(var3, var3))),
   Self.Env := EnvInsert(Self.Env, 'cond', CreateFunType(Self.Bool, CreateFunType(v3, CreateFunType(v3, v3))));
   // "zero" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Bool),
   Self.Env := EnvInsert(Self.Env, 'zero', CreateFunType(Self.Int, Self.Bool));
   // "pred" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer),
   Self.Env := EnvInsert(Self.Env, 'pred', CreateFunType(Self.Int, Self.Int));
   // "times"-> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer))
   Self.Env := EnvInsert(Self.Env, 'times', CreateFunType(Self.Int, CreateFunType(Self.Int, Self.Int)));
   
   //Self.PrintEnvironment(Self.Env);
end;

function TGoofyTypeSystem.GetExprType(ast: TSyntaxNode): TType;
begin
   Result := Self.Analyse(ast, Self.Env);
end;

function TGoofyTypeSystem.GetExprTypeStr(ast: TSyntaxNode): String;
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
