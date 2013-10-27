unit HindleyMilner;
{$mode objfpc}{$H+}


interface


uses SysUtils;

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

   TType = class abstract
      function ToStr: String; virtual; abstract;
   end;
      
   TVariable = class(TType)
   private
      Id: Integer;
      Name: String;
      Instance: TType;
      IsDefined: Boolean;
   public
      constructor Create(id_: Integer);
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
   
   function FunType(from: TType; into: TType): TOper;
   
   
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

constructor TVariable.Create(id_: Integer);
begin
   Self.Id := id_;
   Self.Name := 'random name'; // TODO: include next name generator here
   Self.IsDefined := False;
   inherited Create;
end;

function TVariable.ToStr: String;
begin
   if Self.IsDefined then
      Result := Self.Instance.ToStr
   else
      Result := Self.Name;
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
   i, len: Integer;
   acc: String;
begin
   len := Length(Self.Args);
   if len = 0 then
      Result := Self.Name
   else if len = 2 then
      Result := '(' + Self.Args[0].ToStr + ' ' + Self.Name + ' ' + Self.Args[1].ToStr + ')'
   else
      Result := 'something long'; // TODO: join args into one string
end;

constructor TOper.Create(n: String; a: array of TType);
var i: Integer;
begin
   Self.Name := n;
   SetLength(Self.Args, Length(a));
   for i := 1 to Length(a) do
      Self.Args[i] := a[i];
end;

function FunType(from: TType; into: TType): TOper;
var
   args: array of TType;
begin
   SetLength(args, 2);
   args[1] := from;
   args[2] := into;
   Result := Toper.Create('->', args);
end;


initialization
   
   
end.
