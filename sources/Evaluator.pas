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
   ifc: TIfThenElse;
   casec: TCaseOf;
   value, litval: TValue;
   matched: Boolean;
   i: Integer;
   id: TIdentifier;
   pair: TPairLiteral;
   v, fun: TValue;
   newEnv: TValueEnvironment;
   fv: TFunctionValue;
   arg, fval, sval: TValue;
   cond: TBooleanValue;
   bfv: TBuiltinFunctionValue;
   pabfv: TPABuiltinFunctionValue;
begin
   if (ast is TIntegerLiteral) then
      Result := IntegerV((ast as TIntegerLiteral).Value)
   else if (ast is TBooleanLiteral) then
      Result := BooleanV((ast as TBooleanLiteral).Value)
   else if (ast is TUnitLiteral) then
      Result := UnitV
   else if (ast is TIdentifier) then
      Result := EnvLookup(env, (ast as TIdentifier).Name)
   else if (ast is TPairLiteral) then
   begin
      pair := ast as TPairLiteral;
      fval := Evaluate(pair.Fst, env);
      sval := Evaluate(pair.Snd, env);
      Result := PairV(fval, sval);
   end
   else if (ast is TIfThenElse) then
   begin
      ifc := ast as TIfThenElse;
      cond := Evaluate(ifc.Cond, env) as TBooleanValue;
      if cond.Value then
         Result := Evaluate(ifc.Then_, env)
      else
         Result := Evaluate(ifc.Else_, env);
   end
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
      fv := v as TFunctionValue;
      fv.Env := EnvInsert(fv.Env, letrec.Variable, fv);
      // bind it's value to a let.Variable in a new environment
      newEnv := EnvInsert(env, letrec.Variable, fv);
      // evaluate let.Body with new environment
      Result := Evaluate(letrec.Body, newEnv);
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
      arg := Evaluate(apply.Argument, env);
      if (fun is TFunctionValue) then
      begin
         // apply function
         fv := fun as TFunctionValue;
         newEnv := EnvInsert(fv.Env, fv.Lambda.Variable, arg);
         Result := Evaluate(fv.Lambda.Body, newEnv);
      end
      else if (fun is TBuiltinFunctionValue) then
      begin
         bfv := fun as TBuiltinFunctionValue;
         Result := Self.Builtins.ApplyBuiltin(bfv.Name, arg, env);
      end
      else if (fun is TPABuiltinFunctionValue) then
      begin
         pabfv := fun as TPABuiltinFunctionValue;
         Result := Self.Builtins.ApplyPABuiltin(pabfv.Name, pabfv.DefVal, arg, env);
      end
      else
         raise EEvalError.Create('Value ' + fun.ToStr + ' is not a function and cannot be applied');
   end
   else if (ast is TCaseOf) then
   begin
      casec := ast as TCaseOf;
      value := Evaluate(casec.Expr, env);
      Matched := false;
      for i := 0 to High(casec.Clauses) do
      begin
         // if pattern is literal and equals to `value` -> evaluate clause body
         // if pattern is identifier _ -> evaluate clause body
         // if pattern is identifier -> bind identifier to `value` and evaluate body with new environment   
         if casec.Clauses[i].Pattern is TLiteral then
         begin
            litVal := Evaluate(casec.Clauses[i].Pattern, env);
            if EqualValues(litVal, value) then
            begin
               Result := Evaluate(casec.Clauses[i].Then_, env);
               Matched := True;
               Break;
            end;
         end
         else if casec.Clauses[i].Pattern is TIdentifier then
         begin
            id := casec.Clauses[i].Pattern as TIdentifier;
            if id.Name = '_' then
            begin
               Result := Evaluate(casec.Clauses[i].Then_, env);
               Matched := True;
               Break;
            end
            else
            begin
               newEnv := EnvInsert(env, id.Name, value);
               Result := Evaluate(casec.Clauses[i].Then_, newEnv);
               Matched := True;
               Break;
            end;
         end
         else if casec.Clauses[i].Pattern is TPairLiteral then
            raise EEvalError.Create('Pair literals cannot appear in case-patterns')
      end;
      // if no clause matched - report an error
      if not Matched then
         raise EEvalError.Create('Non-exhaustive patterns in case');
   end
   else
      raise EEvalError.Create('Cannot evaluate this kind of AST');
end;

function TEvaluator.Evaluate(ast: TExpression): TValue;
begin
   Result := Evaluate(ast, Self.BuiltinValues);
end;

initialization
   
end.
