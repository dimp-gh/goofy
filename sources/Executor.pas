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
   public
      Eval: TEvaluator;
      TypeSystem: TGoofyTypeSystem;
      function Typecheck(e: TExpression): TType;
      procedure Execute(ast: TStatement);
      constructor Create(bs: TGoofyBuiltins);
      function GetValue(name: String): TValue;
      procedure LoadModule(module: TModule);
      procedure PrintTypeEnv;
      procedure PrintValueEnv;
   end;

implementation

procedure TGoofyExecutor.Execute(ast: TStatement);
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
   Result := Self.TypeSystem.Analyse(e, Self.TypeEnv);
end;

function TGoofyExecutor.GetValue(name: String): TValue;
begin
   if EnvFind(Self.ValueEnv, name) then
      Result := EnvLookup(Self.ValueEnv, name)
   else
      raise EExecError.Create('Unbound identifier ' + name);
end;

procedure TGoofyExecutor.LoadModule(module: TModule);
var
   i: Integer;
begin
   for i := 0 to High(module.Stmts) do
      Self.Execute(module.Stmts[i]);
end;

procedure TGoofyExecutor.PrintTypeEnv;
begin
   HMDataStructures.EnvPrint(Self.TypeEnv);
end;

procedure TGoofyExecutor.PrintValueEnv;
begin
   ValueEnvironment.EnvPrint(Self.ValueEnv);
end;

initialization
   
end.
