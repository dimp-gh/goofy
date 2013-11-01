unit HindleyMilner;
{$mode objfpc}{$H+}

interface

uses
   AST,
   HMTypes,
   SysUtils, // for exceptions
   fgl; // for generic data structures

type
   
   // Environment for storing type variables
   TEnvironment = specialize TFPGMap<String,TType>;      
   
   TVariableList = specialize TFPGList<TVariable>;
   
   // TVariableMap = specialize TFPGMap<TVariable,TVariable>;
   // This variant of TVariableMap does not compile because it can't
   // find overloaded comparison operators for TVariables. Even if they're
   // defined in HMTypes. This is very strange, but in FreePascal you just
   // can't overload operators in classes. But specializing TFPGMap requires
   // overloaded comparison operators. So the best thing we can do is to wrap
   // a TVariable into record, for which we actually can overload comparison
   // operators.
   // This is awful and i'm hardly dissapointed about Free Pascal.
   TVariableMap = specialize TFPGMap<TWrappedVariable,TWrappedVariable>;
   
   TTypeSystem = class
   private
      NextVariableId: Integer;
      Generator: PGenerator;
      function GetType(name: String; env: TEnvironment; nongen: TVariableList): TType;
      function IsIntegerLiteral(s: String): Boolean;
      procedure Unify(t1, t2: TType);
      function Fresh(t: TType; nongen: TVariableList): TType;
      function Fresh(t: TType; nongen: TVariableList; maps: TVariableMap): TType;
   public
      Int: TOper;
      Boolean: TOper;
      constructor Create(gen: PGenerator);
      function GenerateVariable: TVariable;
      function Analyse(ast: TSyntaxNode; env: TEnvironment): TType;
      function Analyse(ast: TSyntaxNode; env: TEnvironment; nongen: TVariableList): TType;
   end;

implementation

constructor TTypeSystem.Create(gen: PGenerator);
begin
   Self.NextVariableId := 0;
   Self.Generator := gen;
   Self.Int := TOper.Create('int', []);
   Self.Boolean := TOper.Create('bool', []);
end;

function TTypeSystem.Analyse(ast: TSyntaxNode; env: TEnvironment): TType;
begin
   Result := Self.Analyse(ast, env, TVariableList.Create);
end;

function TTypeSystem.Analyse(ast: TSyntaxNode; env: TEnvironment; nongen: TVariableList): TType;
var
   id: TIdent;
   apply: TApply;
   lambda: TLambda;
   let: TLet;
   letrec: TLetRec;
   funType, argType, resultType, defnType: TType;
   newTypeVar: TVariable;
   newEnv : TEnvironment;
   newNongen: TVariableList;
   i: Integer;
begin
   if (ast is TIdent) then
   begin
      id := ast as TIdent;
      Result := Self.GetType(id.Name, env, nongen);
   end
   else if (ast is TApply) then
   begin
      apply := ast as TApply;
      funType := analyse(apply.Fun, env, nongen);
      argType := analyse(apply.Argument, env, nongen);
      resultType := Self.GenerateVariable;
      Self.Unify(CreateFunType(argType, resultType), funType);
      Result := resultType;
   end
   else if (ast is TLambda) then
   begin
      lambda := ast as TLambda;
      argType := Self.GenerateVariable;
      // copying environment
      newEnv := TEnvironment.Create;
      for i := 0 to env.Count - 1 do
         newEnv.Add(env.GetKey(i), env.GetData(i));
      newEnv.Add(lambda.Variable, argType);
      resultType := analyse(lambda.Body, newEnv, nongen);
      Result := CreateFunType(argType, resultType);
   end
   else if (ast is TLet) then
   begin
      let := ast as TLet;
      defnType := analyse(let.Definition, env, nongen);
      // copying environment
      newEnv := TEnvironment.Create;
      for i := 0 to env.Count - 1 do
         newEnv.Add(env.GetKey(i), env.GetData(i));
      // inserting new type variable from let into environemnt
      newEnv.Add(let.Variable, defnType);
      Result := analyse(let.Body, newEnv, nongen);
   end
   else if (ast is TLetRec) then
   begin
      letrec := ast as TLetRec;
      newTypeVar := Self.GenerateVariable;
      // copying environment
      newEnv := TEnvironment.Create;
      for i := 0 to env.Count - 1 do
         newEnv.Add(env.GetKey(i), env.GetData(i));
      // inserting new type variable from letrec into environemnt
      newEnv.Add(letrec.Variable, newTypeVar);
      // copying nongeneric variables list
      newNongen := TVariableList.Create;
      for i := 0 to nongen.Count - 1 do
         newNongen.Add(nongen.Items[i]);
      // inserting new non-generic variable into nongen
      newNongen.Add(newTypeVar);
      defnType := analyse(letrec.Definition, newEnv, newNonGen);
      Self.Unify(newTypeVar, defnType);
      Result := analyse(letrec.Body, newEnv, nongen);
   end
   else
      Raise Exception.Create('Analysis error: Unknown type of AST node');
end;

function TTypeSystem.GenerateVariable: TVariable;
begin
   Result := TVariable.Create(Self.NextVariableId, @(Self.Generator));
   NextVariableId := NextVariableId + 1;
end;

function TTypeSystem.GetType(name: String; env: TEnvironment; nongen: TVariableList): TType;
var index: Integer;
begin
   if env.Find(name, index) then
      Result := Self.Fresh(env.Data[index], nongen)
   else if Self.IsIntegerLiteral(name) then
      Result := Self.Int
   else
      raise EParseError.Create('Undefined symbol ' + name);
end;

procedure TTypeSystem.Unify(t1,t2: TType);
begin
   
end;

function TTypeSystem.IsIntegerLiteral(s: String): Boolean;
begin
   
end;

function TTypeSystem.Fresh(t: TType; nongen: TVariableList): TType;
var mappings: TVariableMap;
begin
   mappings := TVariableMap.Create;
   Result := Self.Fresh(t, nongen, mappings);
end;

function TTypeSystem.Fresh(t: TType; nongen: TVariableList; maps: TVariableMap): TType;
begin
   
   
end;

initialization   
   
end.
