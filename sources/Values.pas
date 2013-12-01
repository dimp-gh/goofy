unit Values;
{$mode objfpc}{$H+}
interface

uses SysUtils;

type
   TValue = class abstract
   public
      function ToStr: String; virtual; abstract;
   end;
   
   TIntegerValue = class(TValue)
   public
      Value: Integer;
      function ToStr: String; override;
      constructor Create(v: Integer);
   end;
   
   TStringValue = class(TValue)
   public
      Value: String;
      function ToStr: String; override;
      constructor Create(v: String);
   end;
   
   TBooleanValue = class(TValue)
   public
      Value: Boolean; 
      function ToStr: String; override;
      constructor Create(b: Boolean);
   end;
   
   TUnitValue = class(TValue)
   public
      function ToStr: String; override;
      constructor Create;
   end;
   
   // NOTE: this is not really a builtin function value
   // it's just a marker for evaluator to handle it like builtin
   TBuiltinFunctionValue = class(TValue)
   public
      Name: String;
      function ToStr: String; override;
      constructor Create(n: String);
   end;
   
   // Partially applied builtin.
   // This is a way to implement builtin functions like `pair` and `times`
   // Not a pretty solution, but i see no other way.
   TPABuiltinFunctionValue = class(TValue)
   public
      Name: String;
      Defval: TValue;
      function ToStr: String; override;
      constructor Create(n: String; dv: TValue);
   end;
   
   TPairValue = class(TValue)
   public
      Fst: TValue;
      Snd: TValue;
      function ToStr: String; override;
      constructor Create(f, s: TValue);
   end;
      
function IntegerV(v: Integer): TIntegerValue;
function BooleanV(v: Boolean): TBooleanValue;
function BuiltinFunction(name: String): TBuiltinFunctionValue;
function PABuiltinFunction(name: String; v: TValue): TPABuiltinFunctionValue;
function PairV(v1, v2: TValue): TPairValue;
function UnitV: TUnitValue;
function StringV(v: String): TStringValue;

function EqualValues(v1, v2: TValue): Boolean;
function PrintishValue(v: TValue): String;

implementation

function TIntegerValue.ToStr: String;
begin
   Result := IntToStr(Self.Value);
end;

constructor TIntegerValue.Create(v: Integer);
begin
   inherited Create;
   Self.Value := v;   
end;

function TStringValue.ToStr: String;
begin
   Result := '"' + Self.Value + '"';
end;

constructor TStringValue.Create(v: String);
begin
   inherited Create;
   Self.Value := v;
end;

function TBooleanValue.ToStr: String;
begin
   if Self.Value then
      Result := 'true'
   else
      Result := 'false';
end;

constructor TBooleanValue.Create(b: Boolean);
begin
   inherited Create;
   Self.Value := b;
end;

function TUnitValue.ToStr: String;
begin
   Result := '()';
end;

constructor TUnitValue.Create;
begin
   inherited Create;
end;

constructor TBuiltinFunctionValue.Create(n: String);
begin
   inherited Create;
   Self.Name := n;
end;

function TBuiltinFunctionValue.ToStr: String;
begin
   Result := '<built-in function ''' + Self.Name + '''>';
end;

constructor TPABuiltinFunctionValue.Create(n: String; dv: TValue);
begin
   inherited Create;
   Self.Name := n;
   Self.Defval := dv;
end;

function TPABuiltinFunctionValue.ToStr: String;
begin
   Result := '<partially applied built-in function ''' + Name + ''' with value ' + DefVal.ToStr + '>';
end;

function IntegerV(v: Integer): TIntegerValue;
begin
   Result := TIntegerValue.Create(v);
end;

function BooleanV(v: Boolean): TBooleanValue;
begin
   Result := TBooleanValue.Create(v);
end;

function BuiltinFunction(name: String): TBuiltinFunctionValue;
begin
   Result := TBuiltinFunctionValue.Create(name);
end;

function PABuiltinFunction(name: String; v: TValue): TPABuiltinFunctionValue;
begin
   Result := TPABuiltinFunctionValue.Create(name, v);
end;

function TPairValue.ToStr: String;
begin
   Result := '(' + fst.ToStr + ', ' + snd.ToStr + ')';
end;

constructor TPairValue.Create(f, s: TValue);
begin
   Fst := f;
   Snd := s;
end;

function PairV(v1, v2: TValue): TPairValue;
begin
   Result := TPairValue.Create(v1, v2);
end;

function UnitV: TUnitValue;
begin
   Result := TUnitValue.Create;
end;

function StringV(v: String): TStringValue;
begin
   Result := TStringValue.Create(v);
end;

function EqualValues(v1, v2: TValue): Boolean;
// Defines general equality rules for all values
// Used heavilly, for example, in evaluation part of pattern matching
begin
   if (v1 is TIntegerValue) and (v2 is TIntegerValue) then
      Result := (v1 as TIntegerValue).Value = (v2 as TIntegerValue).Value
   else if (v1 is TBooleanValue) and (v2 is TBooleanValue) then
      Result := (v1 as TBooleanValue).Value = (v2 as TBooleanValue).Value
   else if (v1 is TStringValue) and (v2 is TStringValue) then
      Result := (v1 as TStringValue).Value = (v2 as TStringValue).Value
   else if (v1 is TUnitValue) and (v2 is TUnitValue) then
      Result := True
   else Result := False;
end;

function PrintishValue(v: TValue): String;
begin
   if v is TStringValue then
      Result := (v as TStringValue).Value
   else
      Result := v.ToStr;
end;

initialization

end.
