unit Builtins;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   HMTypes, Values, Closures, AST, ValueEnvironment, HMDataStructures;

type
   TGoofyBuiltin = record
      Value: TValue;
      ItsType: TType;
      Name: String;
   end;
   
   EBuiltinError = class(Exception);
   
   TGoofyBuiltins = class
   private
      VarGen: TVariableGenerator;
      Builtins: array of TGoofyBuiltin;
      procedure Insert(b: TGoofyBuiltin);
   public
      constructor Create;
      function GetBuiltinTypes: TTypeEnvironment;
      function GetBuiltinValues: TValueEnvironment;
      function ApplyBuiltin(builtin: String;
                            arg: TExpression;
                            env: TValueEnvironment;
                            evalo: TObject): TValue;
      function ApplyPABuiltin(builtin: String;
                              oldarg: TValue;
                              arg: TExpression;
                              env: TValueEnvironment;
                              evalo: TObject): TValue;
      procedure PrintBuiltins;
   end;
   
function Builtin(n: String; v: TValue; t: TType): TGoofyBuiltin;

implementation

uses Evaluator;

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
   Result := ValueEnvironment.EnvNew;
   for i := 0 to Length(Builtins) - 1 do
      Result := ValueEnvironment.EnvInsert(Result, Builtins[i].Name, Builtins[i].Value);
end;

constructor TGoofyBuiltins.Create;
var
   v1, v2, v3, v4, v5, v6, v7: TTypeVariable;
   // NOTE: there may be a huge bug, because type builtins are created
   // with name generator different from type system's one.
   Int, Bool: TType;
begin
   VarGen := TVariableGenerator.Create;
   Builtins := nil;
   Int := CreateType('Int');
   Bool := CreateType('Bool');
   v1 := VarGen.GenerateVariable;
   v2 := VarGen.GenerateVariable;
   v3 := VarGen.GenerateVariable;
   v4 := VarGen.GenerateVariable;
   v5 := VarGen.GenerateVariable;
   v6 := VarGen.GenerateVariable;
   v7 := VarGen.GenerateVariable;
   
   // built-in values
   Self.Insert(Builtin('true', BooleanV(True), Bool));
   Self.Insert(Builtin('false', BooleanV(False), Bool));
   
   // built-in functions
   Self.Insert(Builtin('pair', BuiltinFunction('pair'), CreateFunType(v1, CreateFunType(v2, CreatePairType(v1, v2)))));
   Self.Insert(Builtin('fst', BuiltinFunction('fst'), CreateFunType(CreatePairType(v3, v4), v3)));
   Self.Insert(Builtin('snd', BuiltinFunction('snd'), CreateFunType(CreatePairType(v5, v6), v6)));
   Self.Insert(Builtin('if', BuiltinFunction('if'), CreateFunType(Bool, CreateFunType(v7, CreateFunType(v7, v7)))));
   Self.Insert(Builtin('zero', BuiltinFunction('zero'), CreateFunType(Int, Bool)));
   Self.Insert(Builtin('pred', BuiltinFunction('pred'), CreateFunType(Int, Int)));
   Self.Insert(Builtin('succ', BuiltinFunction('succ'), CreateFunType(Int, Int)));
   Self.Insert(Builtin('times', BuiltinFunction('times'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('eq', BuiltinFunction('eq'), CreateFunType(Int, CreateFunType(Int, Bool))));
   //Self.Insert(Builtin('eq', BuiltinFunction('eq'), CreateFunType(Bool, CreateFunType(Bool, Bool))));
   // NOTE: Previous line breaks compilation because in value environment keys are names and name 'eq' is allready taken
   // There are many ways to handle that:
   // * First is to create different comparison functions for different types
   // * Second is to implement fucntion overloading.
   //   (Probably through some sort of embedding function signature into environment key)
   
   // built-ins for debugging purposes
   Self.Insert(Builtin('forty-two', IntegerV(42), Int));
   Self.Insert(Builtin('factorial', BuiltinFunction('factorial'), CreateFunType(Int, Int)));
   Self.Insert(Builtin('one-two', PairV(IntegerV(1), IntegerV(2)), CreatePairType(Int, Int)));
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

function TGoofyBuiltins.ApplyBuiltin(builtin: String;
                                     arg: TExpression;
                                     env: TValueEnvironment;
                                     evalo: TObject): TValue;
var
   e: TEvaluator;
   cond: TBooleanValue;
begin
   e := evalo as TEvaluator;
   // dispatching by builtin name
   if (builtin = 'factorial') then
   begin
      // no need to check for arg to have integer type
      // or is it?
      Result := IntegerV(Factorial((e.Evaluate(arg, env) as TIntegerValue).Value));
   end
   else if (builtin = 'zero') then
      Result := BooleanV((e.Evaluate(arg, env) as TIntegerValue).Value = 0)
   else if (builtin = 'succ') then
      Result := IntegerV((e.Evaluate(arg, env) as TIntegerValue).Value + 1)
   else if (builtin = 'pred') then
      Result := IntegerV((e.Evaluate(arg, env) as TIntegerValue).Value - 1)
   else if (builtin = 'if') then
   begin
      // if takes a boolean value (b) and returns a function
      // that takes one argument (x) and returns a function
      // that takes one argument (y) and returns either x or y
      // (according to given boolean value b.
      if (arg as TBooleanValue).Value then
         Result := FunctionV(Lambda('x', Lambda('y', Ident('x'))))
      else
         Result := FunctionV(Lambda('x', Lambda('y', Ident('y'))));
   end
   else if (builtin = 'fst') then
      Result := (e.Evaluate(arg, env) as TPairValue).Fst
   else if (builtin = 'snd') then
      Result := (e.Evaluate(arg, env) as TPairValue).Snd
   else if (builtin = 'pair') then
      Result := PABuiltinFunction('pair', e.Evaluate(arg, env))
   else if (builtin = 'times') then
      Result := PABuiltinFunction('times', e.Evaluate(arg, env))
   else
      raise EBuiltinError.Create('Built-in function ''' + builtin + ''' is not implemented yet');
end;

function TGoofyBuiltins.ApplyPABuiltin(builtin: String;
                                       oldarg: TValue;
                                       arg: TExpression;
                                       env: TValueEnvironment;
                                       evalo: TObject): TValue;
var
   e: TEvaluator;
begin
   e := evalo as TEvaluator;
   if (builtin = 'pair') then
      Result := PairV(oldarg, e.Evaluate(arg, env))
   else if (builtin = 'times') then
      Result := IntegerV((oldarg as TIntegerValue).Value * (e.Evaluate(arg, env) as TIntegerValue).Value)
   else if (builtin = 'ift') then // iftrue - ignoring new argument, returning old one 
      Result := oldArg
   else if (builtin = 'iff') then // iffalse - ignoring old argument, evaluating new one
      Result := e.Evaluate(arg, env)
   else
      raise EBuiltinError.Create('Partially applied built-in function ''' + builtin + ''' is not implemented yet');
end;

function RPadTo(n: Integer; s: String): String;
// right pad given string s with spaces to n characters
begin
   Result := s;
   while Length(Result) < n do
      Result += ' ';
end;

procedure TGoofyBuiltins.PrintBuiltins;
const
   nameDelim = 16;
   typeDelim = 30;
var
   i: Integer;
begin
   writeln(RPadTo(nameDelim, 'Name'), RPadTo(typeDelim, 'Type'), 'Value');
   writeln(RPadTo(nameDelim, '----'), RPadTo(typeDelim, '----'), '-----');
   for i := Low(Builtins) to High(Builtins) do
   begin
      writeln(RPadTo(nameDelim, Builtins[i].Name),
              RPadTo(typeDelim, Builtins[i].ItsType.ToStr),
              Builtins[i].Value.ToStr);
      VarGen.ResetNameGenerator;
   end;
end;

initialization
   
end.
