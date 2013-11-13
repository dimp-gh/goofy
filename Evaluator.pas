unit Evaluator;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   Values, AST, EvaluatorDataStructures, Builtins;

type
   EEvalError = Exception;
   
   TEvaluator = class
   private
      Builtins: TValueEnvironment;
   public
      function Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
      function Evaluate(ast: TExpression): TValue;
      constructor Create(bs: TGoofyBuiltins);
   end;			    
   
implementation

constructor TEvaluator.Create(bs: TGoofyBuiltins);
begin
   Self.Builtins := bs.GetBuiltinValues;
end;

function TEvaluator.Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
var
   let: TLet;
   v: TValue;
   newEnv: TValueEnvironment;
begin
   if (ast is TIntegerLiteral) then
      Result := IntegerV((ast as TIntegerLiteral).Value)
   else if (ast is TIdentifier) then
      Result := EnvLookup(env, (ast as TIdentifier).Name)
   else if (ast is TLet) then
   begin
      let := ast as TLet;
      // evaluate let.Definition
      v := Evaluate(let.Definition, env);
      // bind it's value to a let.Variable in a new environment
      newEnv := EnvInsert(env, let.Variable, v);
      // evaluate let.Body with new environment
      Result := Evaluate(let.Body, newEnv);
   end
   else
      raise EEvalError.Create('AST is not number nor identifier nor let');
end;

function TEvaluator.Evaluate(ast: TExpression): TValue;
begin
   Result := Evaluate(ast, Self.Builtins);
end;

initialization
   
end.
