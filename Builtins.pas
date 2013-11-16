unit Builtins;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   HMTypes, Values, AST, EvaluatorDataStructures, HMDataStructures;

type
   TGoofyBuiltin = record
      Value: TValue;
      ItsType: TType;
      Name: String;
   end;
   
   EBuiltinError = class(Exception);
   
   TGoofyBuiltins = class
   private
      Builtins: array of TGoofyBuiltin;
      procedure Insert(b: TGoofyBuiltin);
   public
      constructor Create;
      function GetBuiltinTypes: TTypeEnvironment;
      function GetBuiltinValues: TValueEnvironment;
      function ApplyBuiltin(builtin: String; arg: TValue; env: TValueEnvironment): TValue;
   end;
   
function Builtin(n: String; v: TValue; t: TType): TGoofyBuiltin;

implementation

function Builtin(n: String; v: TValue; t: TType): TGoofyBuiltin;
begin
   Result.Value := v;
   Result.ItsType := t;
   Result.Name := n;
end;

function TGoofyBuiltins.GetBuiltinTypes: TTypeEnvironment;
var
   i: Integer;
begin
   Result := HMDataStructures.EnvNew;
   for i := 0 to Length(Builtins) - 1 do
      Result := HMDataStructures.EnvInsert(Result, Builtins[i].Name, Builtins[i].ItsType);
end;

function TGoofyBuiltins.GetBuiltinValues: TValueEnvironment;
var
   i: Integer;
begin
   Result := EvaluatorDataStructures.EnvNew;
   for i := 0 to Length(Builtins) - 1 do
      Result := EvaluatorDataStructures.EnvInsert(Result, Builtins[i].Name, Builtins[i].Value);
end;

constructor TGoofyBuiltins.Create;
var
   v1, v2, v3: TTypeVariable;
   vg: TVariableGenerator;
   // NOTE: there may be a huge bug, because type builtins are created
   // with name generator different from type system's one.
   Int, Bool: TType;
begin
   vg := TVariableGenerator.Create;
   Self.Builtins := nil;
   Int := CreateType('int');
   Bool := CreateType('bool');
   v1 := vg.GenerateVariable;
   v2 := vg.GenerateVariable;
   v3 := vg.GenerateVariable;

   // built-in values
   Self.Insert(Builtin('true', BooleanV(True), Bool));
   Self.Insert(Builtin('false', BooleanV(False), Bool));
   
   // built-in functions
   Self.Insert(Builtin('pair', BuiltinFunction('pair'), CreateFunType(v1, CreateFunType(v2, CreatePairType(v1, v2)))));
   Self.Insert(Builtin('cond', BuiltinFunction('cond'), CreateFunType(Bool, CreateFunType(v3, CreateFunType(v3, v3)))));
   Self.Insert(Builtin('zero', BuiltinFunction('zero'), CreateFunType(Int, Bool))); // X
   Self.Insert(Builtin('pred', BuiltinFunction('pred'), CreateFunType(Int, Int))); // X
   Self.Insert(Builtin('succ', BuiltinFunction('succ'), CreateFunType(Int, Int))); // X
   Self.Insert(Builtin('times', BuiltinFunction('times'), CreateFunType(Int, CreateFunType(Int, Int))));
   
   // built-ins for debugging purposes
   Self.Insert(Builtin('forty-two', IntegerV(42), Int)); // X
   Self.Insert(Builtin('factorial', BuiltinFunction('factorial'), CreateFunType(Int, Int))); // X
end;

procedure TGoofyBuiltins.Insert(b: TGoofyBuiltin);
var
   len: Integer;
begin
   len := Length(Self.Builtins);
   SetLength(Self.Builtins, len + 1);
   Self.Builtins[len] := b;
end;

function Factorial(n: Integer): Integer;
var
   i: Integer;
begin
   Result := 1;
   for i := 2 to n do
      Result := Result * i;
end;

function TGoofyBuiltins.ApplyBuiltin(builtin: String; arg: TValue; env: TValueEnvironment): TValue;
begin
   // dispatching by builtin name
   if (builtin = 'factorial') then
   begin
      // no need to check for arg to have integer type
      // or is it?
      Result := IntegerV(Factorial((arg as TIntegerValue).Value));
   end
   else if (builtin = 'zero') then
      Result := BooleanV((arg as TIntegerValue).Value = 0)
   else if (builtin = 'succ') then
      Result := IntegerV((arg as TIntegerValue).Value + 1)
   else if (builtin = 'pred') then
      Result := IntegerV((arg as TIntegerValue).Value - 1)
   else if (builtin = 'cond') then
      // cond takes a boolean value and returns a function that takes one argument (x)
      // and returns a function that takes one argument (y) and returns either x or y
      // according to a given boolean value
      if (arg as TBooleanValue).Value then //pretty cool idea, but it just doesn't work now
         Result := FunctionV(Lambda('x', Lambda('y', Ident('x'))))
      else
         Result := FunctionV(Lambda('x', Lambda('y', Ident('y'))))
   else
      raise EBuiltinError('Builtin ' + builtin + ' is not implemented yet');
end;

initialization
   
end.
