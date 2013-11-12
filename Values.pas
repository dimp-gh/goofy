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
   
   TFunctionValue = class(TValue)
   public
      Lambda: TLambda;
      function ToStr: String; override;
      constructor Create(l: TLambda);
   end;
   
function IntegerV(v: Integer): TIntegerValue;
function BooleanV(v: Boolean): TBooleanValue;
function FunctionV(v: TLambda): TFunctionValue;

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

function TFunctionValue.ToStr: String;
begin
   Result := '<function>';
end;

constructor TFunctionValue.Create(l: TLambda);
begin
   inherited Create;
   Self.Lambda := l;
end;

function IntegerV(v: Integer): TIntegerValue;
begin
   Result := TIntegerValue.Create(v);
end;

function BooleanV(v: Boolean): TBooleanValue;
begin
   Result := TBooleanValue.Create(v);
end;

function FunctionV(v: TLambda): TFunctionValue;
begin
   Result := TFunctionValue.Create(v);
end;

initialization

end.
