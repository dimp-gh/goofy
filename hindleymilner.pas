unit HindleyMilner;
{$mode objfpc}{$H+}


interface


uses SysUtils, // for exceptions
     fgl; // for generic Map<String,TType> and List

type
   TSyntaxNode = class abstract
      function ToStr: String; virtual; abstract;
   end;

   TLambda = class(TSyntaxNode)
   private
      Variable: String;
      Body: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(v: String; b: TSyntaxNode);
   end;

   TIdent = class(TSyntaxNode)
   private
      Name: String;
   public
      function ToStr: String; override;
      constructor Create(n: String);
   end;

   TApply = class(TSyntaxNode)
   private
      Fun: TSyntaxNode;
      Argument: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(fn: TSyntaxNode; arg: TSyntaxNode);
   end;

   TLet = class(TSyntaxNode)
   private
      Variable: String;
      Definition: TSyntaxNode;
      Body: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(v: String; defn: TSyntaxNode; b: TSyntaxNode);
   end;

   TLetRec = class(TSyntaxNode)
   private
      Variable: String;
      Definition: TSyntaxNode;
      Body: TSyntaxNode;
   public
      function ToStr: String; override;
      constructor Create(v: String; defn: TSyntaxNode; b: TSyntaxNode);
   end;
   
   TSyntaxNodeClass = Class of TSyntaxNode;

   ETypeError = class(Exception);
   EParseError = class(Exception);
   
   PGenerator = ^TGenerator;

   TType = class(TObject)
      function ToStr: String; virtual; abstract;
   end;
   
   TVariable = class(TType)
   private
      Name: String;
      Namegen: PGenerator;
      Instance: TType;
      IsDefined: Boolean;
      function GetName: String;
   public
      constructor Create(ng: PGenerator);
      function ToStr: String; override;
      procedure SetInstance(inst: TType);
      function GetInstance: TType;
   end;
   
   TOper = class(TType)
   private
      Name: String;
      Args: array of TType;
   public
      function ToStr: String; override;
      constructor Create(n: String; a: array of TType);
   end;
   
   // Environment for storing type variables
   TEnvironment = specialize TFPGMap<String,TType>;
   
   // Name generator for variables
   // Every TVariable, when created, receives a pointer to TGenerator instance.
   // TVariable uses pointer to this generator to calculate its name in a lazy way.
   // Generator ensures that no variable gets the same name.
   TGenerator = class(TObject)
   private
      NextName: Char;
   public
      constructor Create(initialName: Char = 'a');
      function GenerateName: String;
   end;
      
   TVariableList = specialize TFPGList<TVariable>;
   
   TTypeSystem = class
   private
      Generator: PGenerator;
      function GetType(name: String; env: TEnvironment; nongen: TVariableList): TType;
   public
      Int: TOper;
      Boolean: TOper;
      constructor Create(gen: PGenerator);
      function GenerateVariable: TVariable;
      procedure Unify(t1, t2: TType);
      function Analyse(ast: TSyntaxNode; env: TEnvironment): TType;
      function Analyse(ast: TSyntaxNode; env: TEnvironment; nongen: TVariableList): TType;
   end;
   
function CreateFunType(from: TType; into: TType): TOper;

implementation


constructor TLambda.Create(v: String; b: TSyntaxNode);
begin
   Self.Variable := v;
   Self.Body := b;
   inherited Create;
end;

function TLambda.ToStr: String;
begin
   Result := '(fn ' + Self.Variable + ' => ' + Self.Body.ToStr + ')';
end;

constructor TIdent.Create(n: String);
begin
   Self.Name := n;
   inherited Create;
end;

function TIdent.ToStr: String;
begin
   Result := Self.Name;
end;

constructor TApply.Create(fn, arg: TSyntaxNode);
begin
   Self.Fun := fn;
   Self.Argument := arg;
   inherited Create;
end;

function TApply.ToStr: String;
begin
   Result := '(' + Self.Fun.ToStr + ' ' + Self.Argument.ToStr + ')';
end;

constructor TLet.Create(v: String; defn, b: TSyntaxNode);
begin
   Self.Variable := v;
   Self.Definition := defn;
   Self.Body := b;
   inherited Create;
end;

function TLet.ToStr: String;
begin
   Result := '(' + 'let ' + Self.Variable + ' = ' + Self.Definition.ToStr + ' in ' + Self.Body.ToStr + ')';
end;

constructor TLetRec.Create(v: String; defn, b: TSyntaxNode);
begin
   Self.Variable := v;
   Self.Definition := defn;
   Self.Body := b;
   inherited Create;
end;

function TLetRec.ToStr: String;
begin
   Result := '(' + 'letrec ' + Self.Variable + ' = ' + Self.Definition.ToStr + ' in ' + Self.Body.ToStr + ')';
end;

constructor TGenerator.Create(initialName: Char = 'a');
begin
   Self.NextName := initialName;
end;

function TGenerator.GenerateName: String;
begin
   Result := Self.NextName;
   Self.NextName := Char(Integer(Self.NextName) + 1);
end;

constructor TVariable.Create(ng: PGenerator);
begin
   Self.Namegen := ng;
   Self.Name := '';
   Self.IsDefined := False;
   inherited Create;
end;

function TVariable.GetName: String;
begin
   if Self.Name = '' then
   begin
      if Namegen <> nil then
         Name := Namegen^.GenerateName
      else
         Raise Exception.Create('Name generator for type variables is undefined');
   end;
   Result := Self.Name;
end;

function TVariable.ToStr: String;
begin
   if Self.IsDefined then
      Result := Self.Instance.ToStr
   else
      Result := Self.GetName;
end;

procedure TVariable.SetInstance(inst: TType);
begin
   Self.Instance := inst;
   Self.IsDefined := True;
end;

function TVariable.GetInstance: TType;
begin
   if Self.ISDefined then
      Result := Self.Instance
   else
      Raise Exception.Create('Get on undefined instance');
end;

function TOper.ToStr: String;
var
   i, len: Integer;
   t1, t2: String;
   acc: String;
begin
   len := Length(Self.Args);
   if len = 0 then
      Result := Self.Name
   else if len = 2 then
   begin
      t1 := Self.Args[0].ToStr;
      t2 := Self.Args[1].ToStr;
      Result := '(' + t1 + ' ' + Self.Name + ' ' + t2 + ')';
   end
   else
   begin
      acc := Self.Name;
      for i := 0 to len - 1 do
         acc := acc + ' ' + Self.Args[i].ToStr;
      Result := acc;
   end;
end;

constructor TOper.Create(n: String; a: array of TType);
var i: Integer;
begin
   Self.Name := n;
   SetLength(Self.Args, Length(a));
   for i := 0 to Length(Self.Args) - 1 do
   begin
      Self.Args[i] := a[i];
   end;
end;

constructor TTypeSystem.Create(gen: PGenerator);
begin
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
   Result := TVariable.Create(@(Self.Generator));
end;

function TTypeSystem.GetType(name: String; env: TEnvironment; nongen: TVariableList): TType;
begin
   
end;

procedure TTypeSystem.Unify(t1,t2: TType);
begin
   
end;

function CreateFunType(from: TType; into: TType): TOper;
var
   args: array of TType;
begin
   SetLength(args, 2);
   args[0] := from;
   args[1] := into;
   Result := Toper.Create('->', args);
end;

initialization
   
   
end.
