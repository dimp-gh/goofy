unit Evaluator;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   Values, Closures, AST, ValueEnvironment, Builtins;

type
   EEvalError = class(Exception);
   
   TEvaluator = class
   private
      BuiltinValues: TValueEnvironment;
      Builtins: TGoofyBuiltins;
   public
      function Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
      function Evaluate(ast: TExpression): TValue;
      constructor Create(bs: TGoofyBuiltins);
   end;			    
   
implementation

constructor TEvaluator.Create(bs: TGoofyBuiltins);
begin
   Self.Builtins := bs;
   Self.BuiltinValues := bs.GetBuiltinValues;
end;

function TEvaluator.Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
var
   let: TLet;
   letrec: TLetRec;
   lambda: TLambda;
   apply: TApply;
   v, fun: TValue;
   newEnv: TValueEnvironment;
   fv: TFunctionValue;
   arg: TValue;
   bfv: TBuiltinFunctionValue;
   pabfv: TPABuiltinFunctionValue;
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
   else if (ast is TLetRec) then
   begin
      letrec := ast as TLetRec;
      // evaluate let.Definition
      v := Evaluate(letrec.Definition, env);
      if (v is TFunctionValue) then
      begin
         fv := v as TFunctionValue;
         fv.Env := EnvInsert(fv.Env, letrec.Variable, fv);
         // bind it's value to a let.Variable in a new environment
         newEnv := EnvInsert(env, letrec.Variable, fv);
         // evaluate let.Body with new environment
         Result := Evaluate(letrec.Body, newEnv);
      end
      else
         raise EEvalError.Create('letrec didn''t receive function in binding');
   end
   else if (ast is TLambda) then
   begin
      lambda := ast as TLambda;
      // Creating closure with current environment
      Result := FunctionV(lambda, EnvCopy(env));
   end
   else if (ast is TApply) then
   begin
      apply := ast as TApply;
      fun := Evaluate(apply.Fun, env);
      if (fun is TFunctionValue) then
      begin
         // evaluate argument
         arg := Evaluate(apply.Argument, env);
         // apply function
         fv := fun as TFunctionValue;
         newEnv := EnvInsert(fv.Env, fv.Lambda.Variable, arg);
         Result := Evaluate(fv.Lambda.Body, newEnv);
      end
      else if (fun is TBuiltinFunctionValue) then
      begin
         bfv := fun as TBuiltinFunctionValue;
         Result := Self.Builtins.ApplyBuiltin(bfv.Name, apply.Argument, env, Self);
      end
      else if (fun is TPABuiltinFunctionValue) then
      begin
         pabfv := fun as TPABuiltinFunctionValue;
         Result := Self.Builtins.ApplyPABuiltin(pabfv.Name, pabfv.DefVal, apply.Argument, env, Self);
      end
      else
         raise EEvalError.Create('Value ' + fun.ToStr + ' is not a function and cannot be applied');
   end
   else
      raise EEvalError.Create('AST has unknown type');
end;

function TEvaluator.Evaluate(ast: TExpression): TValue;
begin
   Result := Evaluate(ast, Self.BuiltinValues);
end;

initialization
   
end.
