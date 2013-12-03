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
      procedure Execute(ast: TStatement;
                        var valueE: TValueEnvironment;
                        var typeE: TTypeEnvironment);
      function Evaluate(ast: TExpression;
                        var valueE: TValueEnvironment;
                        var typeE: TTypeEnvironment): TValue;
      function MatchClause(clause: TClause;
                           value: TValue;
                           var valueE: TValueEnvironment;
                           var typeE: TTypeEnvironment;
                           var matched: Boolean): TValue;
   public
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

procedure TGoofyExecutor.Execute(ast: TStatement; var valueE: TValueEnvironment; var typeE: TTypeEnvironment);
var
   valDecl: TValueDeclaration;
   value: TValue;
begin
   if ast is TValueDeclaration then
   begin
      valDecl := ast as TValueDeclaration;
      if not Assigned(valDecl.Expr.Type_) then
         raise EExecError.Create('Cannot execute value declaration without type annotation');
      value := Evaluate(valDecl.Expr, valueE, typeE);
      if valDecl.Name <> '_' then
      begin
         valueE := valueEnvironment.EnvInsert(valueE, valDecl.Name, value);
         typeE := HMDataStructures.EnvInsert(typeE, valDecl.Name, valDecl.Expr.Type_);
      end;
   end
   else
      Raise EExecError.Create('Cannot execute unknown kind of AST');
end;

function TGoofyExecutor.Evaluate(ast: TExpression; var valueE: TValueEnvironment; var typeE: TTypeEnvironment): TValue;
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
   newEnv, forkedValueEnv: TValueEnvironment;
   forkedTypeEnv: TTypeEnvironment;
   do_: TDoExpression;
   fv: TFunctionValue;
   arg, fval, sval: TValue;
   cond: TBooleanValue;
   bfv: TBuiltinFunctionValue;
   pabfv: TPABuiltinFunctionValue;
   stmt: TStatement;
   valDecl: TValueDeclaration;
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
      Result := EnvLookup(valueE, (ast as TIdentifier).Name)
   else if (ast is TPairLiteral) then
   begin
      pair := ast as TPairLiteral;
      fval := Evaluate(pair.Fst, valueE, typeE);
      sval := Evaluate(pair.Snd, valueE, typeE);
      Result := PairV(fval, sval);
   end
   else if (ast is TIfThenElse) then
   begin
      ifc := ast as TIfThenElse;
      cond := Evaluate(ifc.Cond, valueE, typeE) as TBooleanValue;
      if cond.Value then
         Result := Evaluate(ifc.Then_, valueE, typeE)
      else
         Result := Evaluate(ifc.Else_, valueE, typeE);
   end
   else if (ast is TLet) then
   begin
      let := ast as TLet;
      // evaluate let.Definition
      v := Evaluate(let.Definition, valueE, typeE);
      // bind it's value to a let.Variable in a new environment
      newEnv := EnvInsert(valueE, let.Variable, v);
      // evaluate let.Body with new environment
      Result := Evaluate(let.Body, newEnv, typeE);
   end
   else if (ast is TLetRec) then
   begin
      letrec := ast as TLetRec;
      // evaluate let.Definition
      v := Evaluate(letrec.Definition, valueE, typeE);
      fv := v as TFunctionValue;
      fv.Env := EnvInsert(fv.Env, letrec.Variable, fv);
      // bind it's value to a let.Variable in a new environment
      newEnv := EnvInsert(valueE, letrec.Variable, fv);
      // evaluate let.Body with new environment
      Result := Evaluate(letrec.Body, newEnv, typeE);
   end
   else if (ast is TLambda) then
   begin
      lambda := ast as TLambda;
      // Creating closure with current environment
      Result := FunctionV(lambda, EnvCopy(valueE));
   end
   else if (ast is TApply) then
   begin
      apply := ast as TApply;
      fun := Evaluate(apply.Fun, valueE, typeE);
      arg := Evaluate(apply.Argument, valueE, typeE);
      if (fun is TFunctionValue) then
      begin
         // apply function
         fv := fun as TFunctionValue;
         newEnv := ValueEnvironment.EnvInsert(fv.Env, fv.Lambda.Variable, arg);
         Result := Evaluate(fv.Lambda.Body, newEnv, typeE);
      end
      else if (fun is TBuiltinFunctionValue) then
      begin
         bfv := fun as TBuiltinFunctionValue;
         Result := Self.Builtins.ApplyBuiltin(bfv.Name, arg, valueE);
      end
      else if (fun is TPABuiltinFunctionValue) then
      begin
         pabfv := fun as TPABuiltinFunctionValue;
         Result := Self.Builtins.ApplyPABuiltin(pabfv.Name, pabfv.DefVal, arg, valueE); // TODO: builtins don't need environments
      end
      else
         raise EEvalError.Create('Value ' + fun.ToStr + ' is not a function and cannot be applied');
   end
   else if (ast is TCaseOf) then
   begin
      casec := ast as TCaseOf;
      value := Evaluate(casec.Expr, valueE, typeE);
      Matched := false;
      for i := 0 to High(casec.Clauses) do
      begin
         Result := MatchClause(casec.Clauses[i], value, valueE, typeE, Matched);
         if Matched then
            break;
      end;
      // if no clause matched - report an error
      if not Matched then
         raise EEvalError.Create('Non-exhaustive patterns in case');
   end
   else if (ast is TDoExpression) then
   begin
      do_ := ast as TDoExpression;
      // fork environments
      forkedValueEnv := ValueEnvironment.EnvCopy(valueE);
      forkedTypeEnv := HMDataStructures.EnvCopy(typeE);
      // evaluate all substatements
      for i := 0 to High(do_.Stmts) do
      begin
         stmt := do_.Stmts[i];
         if (stmt is TValueDeclaration) then
         begin
            valDecl := stmt as TValueDeclaration;
            if not Assigned(valDecl.Expr.Type_) then
               raise EExecError.Create('Cannot execute value declaration without type annotation');
            value := Evaluate(valDecl.Expr, forkedValueEnv, forkedTypeEnv);
            if valDecl.Name <> '_' then
            begin
               forkedValueEnv := valueEnvironment.EnvInsert(forkedValueEnv, valDecl.Name, value);
               forkedTypeEnv := HMDataStructures.EnvInsert(forkedTypeEnv, valDecl.Name, valDecl.Expr.Type_);
            end;
         end
      end;
      // evaluate last expression
      Result := Evaluate(do_.Return, forkedValueEnv, forkedTypeEnv);
   end
   else
      raise EEvalError.Create('Cannot evaluate this kind of AST');
end;

constructor TGoofyExecutor.Create(bs: TGoofyBuiltins);
begin
   Self.Builtins := bs;
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
   vd: TValueDeclaration;
begin
   for i := 0 to High(module.Stmts) do
   begin
      if module.Stmts[i] is TValueDeclaration then
      begin
         vd := module.Stmts[i] as TValueDeclaration;
         Self.TypeCheck(vd.Expr);
         Self.Execute(vd);
      end
      else
         raise EExecError.Create('Module contains unknown kind of statement');
   end;
end;

procedure TGoofyExecutor.PrintTypeEnv;
begin
   HMDataStructures.EnvPrint(Self.TypeEnv);
end;

procedure TGoofyExecutor.PrintValueEnv;
begin
   ValueEnvironment.EnvPrint(Self.ValueEnv);
end;

function TGoofyExecutor.MatchClause(clause: TClause;
                                    value: TValue;
                                    var valueE: TValueEnvironment;
                                    var typeE: TTypeEnvironment;
                                    var matched: Boolean): TValue;
var
   litVal: TValue;
   tmpEnv, finalEnv: TValueEnvironment;
   id: TIdentifier;
   pair: TPairLiteral;
   pairval: TPairValue;
   matchedFst, matchedSnd: Boolean;
begin
   matched := False;
   if clause.Pattern is TLiteral then
   begin
      litVal := Evaluate(clause.Pattern, valueE, typeE);
      if EqualValues(litVal, value) then
      begin
         Result := Evaluate(clause.Then_, valueE, typeE);
         matched := True;
      end;
   end
   else if clause.Pattern is TIdentifier then
   begin
      id := clause.Pattern as TIdentifier;
      if id.Name = '_' then
         finalEnv := valueE
      else
         finalEnv := EnvInsert(valueE, id.Name, value);
      Result := Evaluate(clause.Then_, finalEnv, typeE);
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
         litVal := Evaluate(pair.Fst, valueE, typeE);
         if EqualValues(litVal, pairval.Fst) then
            matchedFst := True;
         tmpEnv := valueE;
      end
      else if pair.Fst is TIdentifier then
      begin
         id := pair.Fst as TIdentifier;
         if id.Name = '_' then
            tmpEnv := valueE
         else
            tmpEnv := EnvInsert(valueE, id.Name, pairval.Fst);
         matchedFst := True;
      end
      else
         raise EEvalError.Create('Unknown type of pattern appeared in case-expression');
      
      if pair.Snd is TLiteral then
      begin
         litVal := Evaluate(pair.Snd, tmpEnv, typeE);
         if EqualValues(litVal, pairval.Snd) then
            matchedSnd := True;
         finalEnv := tmpEnv;
      end
      else if pair.Snd is TIdentifier then
      begin
         id := pair.Snd as TIdentifier;
         if id.Name = '_' then
            finalEnv := tmpEnv
         else
            finalEnv := EnvInsert(tmpEnv, id.Name, pairval.Snd);
         matchedSnd := True;
      end
      else
         raise EEvalError.Create('Unknown type of pattern appeared in case-expression');
      
      if matchedFst and matchedSnd then
      begin
         matched := True;
         Result := Evaluate(clause.Then_, finalEnv, typeE);
      end;
      // raise EEvalError.Create('Pair literals cannot appear in case-patterns');
   end;
end;

function TGoofyExecutor.Evaluate(ast: TExpression): TValue;
begin
   Result := Evaluate(ast, Self.ValueEnv, Self.TypeEnv);
end;

procedure TGoofyExecutor.Execute(ast: TStatement);
begin
   Execute(ast, Self.ValueEnv, Self.TypeEnv);
end;

initialization
   
end.
