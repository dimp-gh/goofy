unit Executor;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   AST, HMDataStructures, ValueEnvironment, Builtins, HMTypes, Values,
   Evaluator, GoofyTypeSystem;

type
   EExecError = class(Exception);
   
   // Executor executes statements and maintains type and value environments between executions
   // Also, executor incapsulates evaluator and typechecker
   TGoofyExecutor = class
   private
      ValueEnv: TValueEnvironment;
      TypeEnv: TTypeEnvironment;
      Eval: TEvaluator;
      TypeSystem: TGoofyTypeSystem;
   public
      function Typecheck(e: TExpression): TType;
      procedure Execute(ast: TStatement);
      constructor Create(bs: TGoofyBuiltins);
      function GetValue(name: String): TValue;
   end;		    
   
implementation

procedure TGoofyExecutor.Execute(ast: TStatement);
// Assuming that AST is already typechecked
var
   valDecl: TValueDeclaration;
   valType: TType;
   value: TValue;
begin
   if ast is TValueDeclaration then
   begin
      valDecl := ast as TValueDeclaration;
      valType := Typecheck(valDecl.Expr);
      value := Eval.Evaluate(valDecl.Expr, Self.ValueEnv);
      Self.ValueEnv := ValueEnvironment.EnvInsert(Self.ValueEnv, valDecl.Name, value);
      Self.TypeEnv := HMDataStructures.EnvInsert(Self.TypeEnv, valDecl.Name, valType);
   end
   else
      Raise EExecError.Create('Cannot execute unknown type of AST');
end;

constructor TGoofyExecutor.Create(bs: TGoofyBuiltins);
begin
   Self.Eval := TEvaluator.Create(bs);
   Self.TypeSystem := TGoofyTypeSystem.Create(bs);
   Self.ValueEnv := bs.GetBuiltinValues;
   Self.TypeEnv := bs.GetBuiltinTypes;
end;

function TGoofyExecutor.Typecheck(e: TExpression): TType;
begin
   Result := Self.TypeSystem.GetExprType(e);
end;

function TGoofyExecutor.GetValue(name: String): TValue;
begin
   if EnvFind(Self.ValueEnv, name) then
      Result := EnvLookup(Self.ValueEnv, name)
   else
      raise EExecError.Create('Unbound identifier ' + name);
end;

initialization
   
end.
