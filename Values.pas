unit Values;
{$mode objfpc}{$H+}
interface

uses SysUtils, AST;

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
   
   TBooleanValue = class(TValue)
   public
      Value: Boolean; 
      function ToStr: String; override;
      constructor Create(b: Boolean);
   end;
      
   // NOTE: this is not really a builtin function value
   // it's just a marker for evaluator to handle it like builtin
   TBuiltinFunctionValue = class(TValue)
   public
      Name: String;
      function ToStr: String; override;
      constructor Create(n: String);
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
function PairV(v1, v2: TValue): TPairValue;
   
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

constructor TBuiltinFunctionValue.Create(n: String);
begin
   inherited Create;
   Self.Name := n;
end;

function TBuiltinFunctionValue.ToStr: String;
begin
   Result := '<built-in function ''' + Self.Name + '''>';
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

initialization

end.
