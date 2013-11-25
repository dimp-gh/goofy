unit Closures;
{$mode objfpc}{$H+}
interface

uses Values, AST, ValueEnvironment;

type
   TFunctionValue = class(TValue)
   public
      Lambda: TLambda;
      Env: TValueEnvironment;
      function ToStr: String; override;
      constructor Create(l: TLambda; e: TValueEnvironment = nil);
   end;
   
function FunctionV(v: TLambda): TFunctionValue;
function FunctionV(v: TLambda; e: TValueEnvironment): TFunctionValue;
function IdentityFunctionV: TFunctionValue;

implementation

function TFunctionValue.ToStr: String;
begin
   Result := '<function value>';
end;

constructor TFunctionValue.Create(l: TLambda; e: TValueEnvironment = nil);
begin
   inherited Create;
   Self.Lambda := l;
   Self.Env := e;
end;

function FunctionV(v: TLambda): TFunctionValue;
begin
   Result := TFunctionValue.Create(v);
end;

function FunctionV(v: TLambda; e: TValueEnvironment): TFunctionValue;
begin
   Result := TFunctionValue.Create(v, e);
end;

function IdentityFunctionV: TFunctionValue;
begin
   Result := FunctionV(Lambda('x', Identifier('x')));
end;

initialization

end.
