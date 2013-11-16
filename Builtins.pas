unit Builtins;
{$mode objfpc}{$H+}
interface

uses
   HMTypes, Values, AST, EvaluatorDataStructures, HMDataStructures;

type
   TGoofyBuiltin = record
      Value: TValue;
      ItsType: TType;
      Name: String;
   end;
   
   TGoofyBuiltins = class
   private
      Builtins: array of TGoofyBuiltin;
      procedure Insert(b: TGoofyBuiltin);
   public
      constructor Create;
      function GetBuiltinTypes: TTypeEnvironment;
      function GetBuiltinValues: TValueEnvironment;
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
   
   Self.Insert(Builtin('true', BooleanV(True), Bool));
   Self.Insert(Builtin('false', BooleanV(False), Bool));
   Self.Insert(Builtin('pair', EmptyFunction, CreateFunType(v1, CreateFunType(v2, CreatePairType(v1, v2)))));
   Self.Insert(Builtin('cond', EmptyFunction, CreateFunType(Bool, CreateFunType(v3, CreateFunType(v3, v3)))));
   Self.Insert(Builtin('zero', EmptyFunction, CreateFunType(Int, Bool)));
   Self.Insert(Builtin('pred', EmptyFunction, CreateFunType(Int, Int)));
   Self.Insert(Builtin('times', EmptyFunction, CreateFunType(Int, CreateFunType(Int, Int))));
   
   Self.Insert(Builtin('forty-two', IntegerV(42), Int));
   
end;

procedure TGoofyBuiltins.Insert(b: TGoofyBuiltin);
var
   len: Integer;
begin
   len := Length(Self.Builtins);
   SetLength(Self.Builtins, len + 1);
   Self.Builtins[len] := b;
end;

initialization
   
end.
