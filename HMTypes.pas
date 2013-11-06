unit HMTypes;
{$mode delphi}{$H+}
interface

uses
   SysUtils; // for exceptions

type

   ETypeError = class(Exception);
   EParseError = class(Exception);
   
   PNameGenerator = ^TNameGenerator;

   TType = class(TObject)
      function ToStr: String; virtual; abstract;
   end;
   
   TTypeVariable = class(TType)
   private
      Namegen: PNameGenerator;
      Instance: TType;      
   public
      Id: Integer;
      Name: String;
      IsDefined: Boolean;
      function GetName: String;
      constructor Create(id_: Integer; ng: PNameGenerator);
      function ToStr: String; override;
      procedure SetInstance(inst: TType);
      function GetInstance: TType;
   end;
      
   TParameterizedType = class(TType)
   public
      Name: String;
      Args: array of TType;
      function ToStr: String; override;
      constructor Create(n: String; a: array of TType);
   end;

   // Name generator for type variables
   // Every TTypeVariable, when created, receives a pointer to TNameGenerator instance.
   // TTypeVariable uses pointer to this generator to calculate its name in a lazy way.
   // NameGenerator ensures that no variable gets the same name.
   TNameGenerator = class(TObject)
   private
      NextName: Char;
   public
      constructor Create(initialName: Char = 'a');
      function GenerateName: String;
   end;
   
function CreateFunType(from: TType; into: TType): TParameterizedType;
function CreatePairType(t1,t2: TType): TParameterizedType;

implementation

constructor TNameGenerator.Create(initialName: Char = 'a');
begin
   Self.NextName := initialName;
end;

function TNameGenerator.GenerateName: String;
begin
   Result := Self.NextName;
   Self.NextName := Char(Integer(Self.NextName) + 1);
end;

constructor TTypeVariable.Create(id_: Integer; ng: PNameGenerator);
begin
   Self.Id := id_;
   Self.Namegen := ng;
   Self.Name := '';
   Self.IsDefined := False;
   inherited Create;
end;

function TTypeVariable.GetName: String;
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

function TTypeVariable.ToStr: String;
begin
   if Self.IsDefined then
      Result := Self.Instance.ToStr
   else
      Result := Self.GetName;
end;

procedure TTypeVariable.SetInstance(inst: TType);
begin
   Self.Instance := inst;
   Self.IsDefined := True;
end;

function TTypeVariable.GetInstance: TType;
begin
   if Self.IsDefined then
      Result := Self.Instance
   else
      Raise Exception.Create('Get on undefined instance');
end;

function TParameterizedType.ToStr: String;
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
      if (Self.Name = '*') or (Self.Name = '->') then
         Result := '(' + t1 + ' ' + Self.Name + ' ' + t2 + ')'
      else
         Result := Self.Name + ' ' + t1 + ' ' + t2; 
   end
   else
   begin
      acc := Self.Name;
      for i := 0 to len - 1 do
         acc := acc + ' ' + Self.Args[i].ToStr;
      Result := acc;
   end;
end;

constructor TParameterizedType.Create(n: String; a: array of TType);
var i: Integer;
begin
   Self.Name := n;
   SetLength(Self.Args, Length(a));
   for i := 0 to Length(Self.Args) - 1 do
   begin
      Self.Args[i] := a[i];
   end;
end;

function CreateFunType(from: TType; into: TType): TParameterizedType;
var
   args: array of TType;
begin
   SetLength(args, 2);
   args[0] := from;
   args[1] := into;
   Result := TParameterizedType.Create('->', args);
end;

function CreatePairType(t1,t2: TType): TParameterizedType;
var
   args: array of TType;
begin
   SetLength(args, 2);
   args[0] := t1;
   args[1] := t2;
   Result := TParameterizedType.Create('*', args);
end;


initialization

end.
