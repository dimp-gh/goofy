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

   TType = class abstract
      function ToStr: String; virtual; abstract;
   end;   
      
   TVariable = class(TType)
   private
      Id: Integer;
      Name: String;
      Namegen: PGenerator;
      Instance: TType;
      IsDefined: Boolean;
      function GetName: String;
   public
      constructor Create(id_: Integer; ng: PGenerator);
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
   
   TTypeClass = Class of TType;
   
   TGenerator = class(TObject)
   private
      NextName: Char;
      NextId: Integer;
   public
      constructor Create(initialName: Char = 'a'; initialId: Integer = 0);
      function GenerateName: String;
      function GenerateVariable: TVariable;
   end;
   
   TEnvironment = specialize TFPGMap<String,TTypeClass>;
   
   TVariableList = specialize TFPGList<TVariable>;
   
   TTypeSystem = class
   private
      Generator: PGenerator;
      function GetType(name: String; env: TEnvironment; nongen: TVariableList): TType;
   public
      Integer: TOper;
      Boolean: TOper;
      constructor Create(gen: PGenerator);
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

constructor TGenerator.Create(initialName: Char = 'a'; initialId: Integer = 0);
begin
   Self.NextName := initialName;
   Self.NextId := initialId;
   writeln('Generator created. Initial name is ', Self.NextName);
end;

function TGenerator.GenerateName: String;
begin
   writeln('GenerateName, name is ', Self.NextName, ', code ', Integer(Self.NextName));
   Result := Self.NextName;
   Self.NextName := Char(Integer(Self.NextName) + 1);
   writeln('Next name is going to be ', Self.NextName, ', code ', Integer(Self.NextName));
end;

function TGenerator.GenerateVariable: TVariable;
begin
   writeln('GenerateVariable, id is ', Self.NextId);
   writeln('self pointer is ', IntToHex(Integer(@Self), 8));
   Result := TVariable.Create(Self.NextId, @Self);
   Self.NextId := Self.NextId + 1;
   writeln('Next id is going to be ', Self.NextId);
end;

constructor TVariable.Create(id_: Integer; ng: PGenerator);
begin
   Self.Id := id_;
   Self.Namegen := ng;
   Self.Name := '';
   Self.IsDefined := False;
   inherited Create;
end;

function TVariable.GetName: String;
begin
   if Self.Name = '' then
   begin
      if Assigned(Self.NameGen) then
         Self.Name := Self.Namegen^.GenerateName
      else
         Raise Exception.Create('Name generator for type variables is nil');
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
   Result := Self.Instance;
end;

function TOper.ToStr: String;
var
   len: Integer;
   t1, t2: String;
begin
   writeln('TOper.ToStr');
   len := Length(Self.Args);
   if len = 0 then
      Result := Self.Name
   else if len = 2 then
   begin
      writeln('len = 2');
      t1 := Self.Args[0].ToStr;
      t2 := Self.Args[1].ToStr;
      writeln('Casted to str successfully, values are ', t1, ' and ', t2);
      Result := '(' + t1 + ' ' + Self.Name + ' ' + t2 + ')';
      writeln('End clause');
   end
   else
      Result := 'something long'; // TODO: join args into one string
   writeln('Returning string value');
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
   Self.Integer := TOper.Create('int', []);
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
   funtype, argtype, resultType: TType;
begin
   if (ast is TIdent) then
   begin
      id := ast as TIdent;
      Result := Self.GetType(id.Name, env, nongen);
   end
   else if (ast is TApply) then
   begin
      apply := ast as TApply;
      funtype := analyse(apply.Fun, env, nongen);
      argtype := analyse(apply.Argument, env, nongen);
      resultType := Self.Generator^.GenerateVariable; // UNSAFE: dereferencing possibly empty pointer
   end
   else if (ast is TLambda) then
   begin
      
   end
   else if (ast is TLet) then
   begin
      
   end
   else if (ast is TLetRec) then
   begin
      
   end
   else
      Raise Exception.Create('Analysis error: Unknown type of AST node');
end;

function TTypeSystem.GetType(name: String; env: TEnvironment; nongen: TVariableList): TType;
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
