unit Evaluator;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   Values, AST, EvaluatorDataStructures;

type
   EEvalError = Exception;
   
   TEvaluator = class
   private
      Builtins: TValueEnvironment;
   public
      function Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
      function Evaluate(ast: TExpression): TValue;
      constructor Create;
   end;   
   
implementation

constructor TEvaluator.Create;
begin
   // TODO: proper builtin mechanisms
   Builtins := EnvNew;
   Builtins := EnvInsert(Builtins, 'forty-two', IntegerV(42));
   Builtins := EnvInsert(Builtins, 'true', BooleanV(True));
   Builtins := EnvInsert(Builtins, 'false', BooleanV(False));
end;

function TEvaluator.Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
begin
   if (ast is TIntegerLiteral) then
      Result := IntegerV((ast as TIntegerLiteral).Value)
   else if (ast is TIdentifier) then
      Result := EnvLookup(env, (ast as TIdentifier).Name)
   else
      raise EEvalError.Create('AST is not number nor identifier');
end;

function TEvaluator.Evaluate(ast: TExpression): TValue;
begin
   Result := Evaluate(ast, Self.Builtins);
end;

initialization
   
end.
