unit HindleyMilner;
{$mode objfpc}{$H+}

interface

uses
   AST,
   HMTypes,
   HMDataStructures,
   SysUtils; // for exceptions

type
   
   // Environment for storing type variables
   // TEnvironment = specialize TFPGMap<String,TType>;      
   
   //TVariableList = array of TVariable;
   
   // TVariableMap = specialize TFPGMap<TVariable,TVariable>;
   // This variant of TVariableMap does not compile because it can't
   // find overloaded comparison operators for TVariables. Even if they're
   // defined in HMTypes. This is very strange, but in FreePascal you just
   // can't overload operators in classes. But specializing TFPGMap requires
   // overloaded comparison operators. So the best thing we can do is to wrap
   // a TVariable into record, for which we actually can overload comparison
   // operators.
   // This is awful and i'm deeply dissapointed about Free Pascal.
   //TVariableMap = specialize TFPGMap<TWrappedVariable,TWrappedVariable>;
   
   TTypeSystem = class
   private
      NextVariableId: Integer;
      Generator: TGenerator;
      function GetType(name: String; env: TEnvironment; nongen: TVariableList): TType;
      procedure Unify(t1, t2: TType);
      function Fresh(t: TType; nongen: TVariableList): TType;
      function Prune(t: TType): TType;
      function IsGeneric(v: TVariable; nongen: TVariableList): Boolean;
      function OccursIn(v: TVariable; types: TTypeList): Boolean;
      function OccursInType(v: TVariable; t: TType): Boolean;
   protected
      procedure PrintEnvironment(env: TEnvironment);
   public
      Int: TOper;
      Bool: TOper;
      constructor Create;
      procedure ResetGenerator;
      function GenerateVariable: TVariable;
      function Analyse(ast: TSyntaxNode; env: TEnvironment): TType;
      function Analyse(ast: TSyntaxNode; env: TEnvironment; nongen: TVariableList): TType;
   end;
   
   function IsIntegerLiteral(s: String): Boolean;
   
implementation

constructor TTypeSystem.Create;
begin
   Self.NextVariableId := 0;
   Self.Generator := TGenerator.Create;
   Self.Int := TOper.Create('int', []);
   Self.Bool := TOper.Create('bool', []);
end;

function TTypeSystem.Analyse(ast: TSyntaxNode; env: TEnvironment): TType;
begin
   Result := Self.Analyse(ast, env, VarListNew);
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
begin
   if (ast is TIdent) then
   begin
      id := ast as TIdent;
      //writeln('DEBUG: analyse(Ident(', id.Name, '))');
      //writeln('DEBUG: current environment:'); PrintEnvironment(env);
      Result := Self.GetType(id.Name, env, nongen);
      //writeln('DEBUG type of ', id.Name, ' is ', Result.ToStr);
   end
   else if (ast is TApply) then
   begin
      //writeln('DEBUG: analyse(Apply)');
      apply := ast as TApply;
      funType := analyse(apply.Fun, env, nongen);
      argType := analyse(apply.Argument, env, nongen);
      resultType := Self.GenerateVariable;
      Self.Unify(CreateFunType(argType, resultType), funType);
      Result := resultType;
   end
   else if (ast is TLambda) then
   begin
      //writeln('DEBUG: analyse(Lambda)');
      lambda := ast as TLambda;
      argType := Self.GenerateVariable;
      // copying environment
      newEnv := EnvInsert(env, lambda.Variable, argType);
      //writeln('DEBUG: old env:'); PrintEnvironment(env);
      //writeln('DEBUG: new env:'); PrintEnvironment(newEnv);
      resultType := analyse(lambda.Body, newEnv, nongen);
      Result := CreateFunType(argType, resultType);
   end
   else if (ast is TLet) then
   begin
      //writeln('DEBUG: analyse(Let)');
      let := ast as TLet;
      defnType := analyse(let.Definition, env, nongen);
      // copying environment
      newEnv := EnvInsert(env, let.Variable, defnType);
      Result := analyse(let.Body, newEnv, nongen);
   end
   else if (ast is TLetRec) then
   begin
      //writeln('DEBUG: analyse(LetRec)');
      letrec := ast as TLetRec;
      newTypeVar := Self.GenerateVariable;
      // copying environment
      newEnv := EnvInsert(env, letrec.Variable, newTypeVar);
      newNonGen := VarListInsert(nongen, newTypeVar);
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
begin
   if EnvFind(env, name) then
      Result := Self.Fresh(EnvLookup(env, name), nongen)
   else if IsIntegerLiteral(name) then
      Result := Self.Int
   else
      raise EParseError.Create('Undefined symbol ' + name);
end;

procedure TTypeSystem.Unify(t1,t2: TType);
var
   pt1, pt2: TType;
   o1, o2: TOper;
   v: TVariable;
   i: Integer;
begin
   pt1 := Self.Prune(t1);
   pt2 := Self.Prune(t2);
   if (pt1 is TVariable) and (pt2 is TVariable) then
   begin
      v := pt1 as TVariable;
      if not((pt2 is TVariable) and ((pt2 as TVariable).Id = v.Id)) then
      begin
         if Self.OccursInType(v, pt2) then
            Raise ETypeError.Create('Recursive unification');
         v.SetInstance(pt2);
      end;
   end
   else if (pt1 is TOper) and (pt2 is TVariable) then
      Self.Unify(pt2, pt1)
   else if (pt1 is TOper) and (pt2 is TOper) then
   begin
      o1 := pt1 as TOper;
      o2 := pt2 as TOper;
      if (o1.Name <> o2.Name) or (Length(o1.Args) <> Length(o2.Args)) then
         raise ETypeError.Create('Type mismatch: ' + o1.ToStr + ' /= ' + o2.ToStr);
      for i := 0 to Length(o1.Args) - 1 do
         Self.Unify(o1.Args[i], o2.Args[i]);
   end;
end;

function TTypeSystem.Fresh(t: TType; nongen: TVariableList): TType;
var
   maps: TVariableMap;
   
   function FreshRec(t: TType; nongen: TVariableList): TType;
   var
      pruned: TType;
      tvar, newVar: TVariable;
      oper: TOper;
      newArgs: array of TType;
      index, len: Integer;
   begin
      //writeln('DEBUG: freshrec');
      pruned := Self.Prune(t);
      if (pruned is TVariable) then
      begin
         tvar := pruned as TVariable;
         if Self.IsGeneric(tvar, nongen) then
         begin
            //writeln('DEBUG: looking for variable #', tvar.Id);
            if VarMapFind(maps, tvar) then
            begin
               Result := VarMapLookup(maps, tvar);
            end
            else
            begin
               newVar := Self.GenerateVariable;
               maps := VarMapInsert(maps, tvar, newVar);
               //writeln('DEBUG: Inserting mapping #', tvar.Id, ' -> #', newVar.Id);
               Result := newVar;
            end;
         end
         else
            Result := tvar;
      end
      else if (pruned is TOper) then
      begin
         oper := pruned as TOper;
         len := Length(oper.Args);
         SetLength(newArgs, len);
         for index := 0 to len - 1 do
            newArgs[index] := FreshRec(oper.Args[index], nongen);
         Result := TOper.Create(oper.Name, newArgs); 
      end
      else
         Raise Exception.Create('Cannot determine type of pruned type tree');
   end;
begin
   //writeln('DEBUG: fresh called');
   maps := VarMapNew;
   Result := FreshRec(t, nongen);
end;


function TTypeSystem.Prune(t: TType): TType;
var
   tvar: TVariable;
   inst: TType;
begin
   if (t is TVariable) and (t as TVariable).IsDefined then
   begin
      tvar := t as TVariable;
      inst := Self.Prune(tvar.GetInstance);
      tvar.SetInstance(inst);
      Result := inst; 
   end
   else
      Result := t;
end;

function TTypeSystem.IsGeneric(v: TVariable; nongen: TVariableList): Boolean;
begin
   Result := not Self.OccursIn(v, VarListToTypeList(nongen));
end;

function TTypeSystem.OccursIn(v: TVariable; types: TTypeList): Boolean;
var
   i: Integer;
begin
   for i := 0 to Length(types) - 1 do
      if Self.OccursInType(v, types[i]) then
      begin
         Result := True;
         Exit;
      end;
   Result := False;
end;

function TTypeSystem.OccursInType(v: TVariable; t: TType): Boolean;
var
   tt: TType;
   oper: TOper;
begin
   tt := Self.Prune(t);
   if (tt is TVariable) and (tt as TVariable = v) then
      Result := True
   else if (tt is TOper) then
   begin
      oper := tt as TOper;
      Result := Self.OccursIn(v, oper.Args);
   end
   else
      Result := False;
end;

procedure TTypeSystem.PrintEnvironment(env: TEnvironment);
begin
   EnvPrint(env);
end;

procedure TTypeSystem.ResetGenerator;
begin
   Self.Generator.Free;
   Self.Generator := TGenerator.Create;
end;

function IsIntegerLiteral(s: String): Boolean;
const
   digits: Set of Char = ['0'..'9'];
var
   i: Integer;
begin
   // Hello, mr. Nazarov. Here we meet again.
   for i := 1 to Length(s) do
      if not(s[i] in digits) then
      begin
         Result := False;
         Exit;
      end;
   Result := True;
end;

initialization   
   
end.
