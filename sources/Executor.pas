unit Executor;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   AST, HMDataStructures, ValueEnvironment, Builtins, HMTypes, Values,
   Closures, GoofyTypeSystem;

type
   EExecError = class(Exception);
   EEvalError = class(Exception);
   
   // Executor executes statements and maintains type and value environments between executions
   // Also, executor incapsulates evaluator and typechecker
   TGoofyExecutor = class
   private
      ValueEnv: TValueEnvironment;
      TypeEnv: TTypeEnvironment;
      Builtins: TGoofyBuiltins;
      function Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
      function MatchClause(clause: TClause;
                           value: TValue;
                           env: TValueEnvironment;
                           var matched: Boolean): TValue;
   public
      //Eval: TEvaluator;
      TypeSystem: TGoofyTypeSystem;
      function Typecheck(e: TExpression): TType;
      function Evaluate(ast: TExpression): TValue;
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
      value := Self.Evaluate(valDecl.Expr, Self.ValueEnv);
      if valDecl.Name <> '_' then
      begin
         Self.ValueEnv := ValueEnvironment.EnvInsert(Self.ValueEnv, valDecl.Name, value);
         Self.TypeEnv := HMDataStructures.EnvInsert(Self.TypeEnv, valDecl.Name, valType);
      end;
   end
   else
      Raise EExecError.Create('Cannot execute unknown type of AST');
end;

constructor TGoofyExecutor.Create(bs: TGoofyBuiltins);
begin
   Self.Builtins := bs;
   //Self.Eval := TEvaluator.Create(bs);
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
      raise EExecError.Create('Undefined identifier ' + name);
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

function TGoofyExecutor.Evaluate(ast: TExpression; env: TValueEnvironment): TValue;
var
   let: TLet;
   letrec: TLetRec;
   lambda: TLambda;
   apply: TApply;
   ifc: TIfThenElse;
   casec: TCaseOf;
   value: TValue;
   matched: Boolean;
   i: Integer;
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
   else if (ast is TStringLiteral) then
      Result := StringV((ast as TStringLiteral).Value)
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
         Result := MatchClause(casec.Clauses[i], value, env, Matched);
         if Matched then
            break;
      end;
      // if no clause matched - report an error
      if not Matched then
         raise EEvalError.Create('Non-exhaustive patterns in case');
   end
   else
      raise EEvalError.Create('Cannot evaluate this kind of AST');
end;

function TGoofyExecutor.MatchClause(clause: TClause;
                                value: TValue;
                                env: TValueEnvironment;
                                var matched: Boolean): TValue;
var
   litVal: TValue;
   newEnv: TValueEnvironment;
   id: TIdentifier;
   pair: TPairLiteral;
   pairval: TPairValue;
   matchedFst, matchedSnd: Boolean;
begin
   matched := False;
   if clause.Pattern is TLiteral then
   begin
      litVal := Evaluate(clause.Pattern, env);
      if EqualValues(litVal, value) then
      begin
         Result := Evaluate(clause.Then_, env);
         matched := True;
      end;
   end
   else if clause.Pattern is TIdentifier then
   begin
      id := clause.Pattern as TIdentifier;
      if id.Name = '_' then
         newEnv := env
      else
         newEnv := EnvInsert(env, id.Name, value);
      Result := Evaluate(clause.Then_, newEnv);
      matched := True;
   end
   else if clause.Pattern is TPairLiteral then
   begin
      matchedFst := False;
      matchedSnd := False;
      pair := clause.Pattern as TPairLiteral;
      pairval := value as TPairValue;
      if pair.Fst is TLiteral then
      begin
         litVal := Evaluate(pair.Fst, env);
         if EqualValues(litVal, pairval.Fst) then
            matchedFst := True;
      end
      else if pair.Fst is TIdentifier then
      begin
         id := pair.Fst as TIdentifier;
         if id.Name = '_' then
            newEnv := env
         else
            newEnv := EnvInsert(env, id.Name, pairval.Fst);
         matchedFst := True;
      end
      else
         raise EEvalError.Create('Unknown type of pattern appeared in case-expression');
      
      if pair.Snd is TLiteral then
      begin
         litVal := Evaluate(pair.Snd, newEnv);
         if EqualValues(litVal, pairval.Snd) then
            matchedSnd := True;
      end
      else if pair.Snd is TIdentifier then
      begin
         id := pair.Snd as TIdentifier;
         if id.Name = '_' then
            newEnv := newEnv
         else
            newEnv := EnvInsert(newEnv, id.Name, pairval.Snd);
         matchedSnd := True;
      end
      else
         raise EEvalError.Create('Unknown type of pattern appeared in case-expression');
      
      if matchedFst and matchedSnd then
      begin
         matched := True;
         Result := Evaluate(clause.Then_, newEnv);
      end;
      // raise EEvalError.Create('Pair literals cannot appear in case-patterns');
   end;
end;

function TGoofyExecutor.Evaluate(ast: TExpression): TValue;
begin
   Result := Evaluate(ast, Self.ValueEnv);
end;


initialization
   
end.
