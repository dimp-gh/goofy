unit HMTypes;
{$mode objfpc}{$H+}

interface

uses
   SysUtils; // for exceptions

type

   ETypeError = class(Exception);
   EParseError = class(Exception);
   
   PGenerator = ^TGenerator;

   TType = class(TObject)
      function ToStr: String; virtual; abstract;
   end;
   
   TVariable = class(TType)
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

   // Name generator for type variables
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
   
operator < (a: TVariable; b: TVariable): Boolean;
   
function CreateFunType(from: TType; into: TType): TOper;

implementation

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

function CreateFunType(from: TType; into: TType): TOper;
var
   args: array of TType;
begin
   SetLength(args, 2);
   args[0] := from;
   args[1] := into;
   Result := Toper.Create('->', args);
end;

operator < (a: TVariable; b: TVariable): Boolean;
begin
   Result := a.Name < b.Name; // Note: comparison should really be done by numeric variable id
end; 

initialization

end.
