unit Executor;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   Values, Closures, AST, ValueEnvironment, Builtins;

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
      procedure Execute(ast: TStatement);
      constructor Create(bs: TGoofyBuiltins);
   end;			    
   
implementation

procedure TGoofyExecutor.Execute(ast: TStatement);
begin
   
end;

constructor TGoofyExecutor.Create(bs: TGoofyBuiltins);
begin
   Self.ValueEnv := bs.GetBuiltinValues;
   Self.TypeEnv := bs.GetBuiltinTypes;
end;

initialization
   
end.
