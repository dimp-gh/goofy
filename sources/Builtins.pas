unit Builtins;
{$mode objfpc}{$H+}
interface

uses
   SysUtils,
   HMTypes, Values, ValueEnvironment, HMDataStructures;

type
   TGoofyBuiltin = record
      Value: TValue;
      ItsType: TType;
      Name: String;
   end;
   
   EBuiltinError = class(Exception);
   EEvaluationStoppedError = class(Exception);
   
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
                            arg: TValue;
                            env: TValueEnvironment): TValue;
      function ApplyPABuiltin(builtin: String;
                              oldarg: TValue;
                              arg: TValue;
                              env: TValueEnvironment): TValue;
      procedure PrintBuiltins;
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
   Result := ValueEnvironment.EnvNew;
   for i := 0 to Length(Builtins) - 1 do
      Result := ValueEnvironment.EnvInsert(Result, Builtins[i].Name, Builtins[i].Value);
end;

constructor TGoofyBuiltins.Create;
var
   vs: TTypeVariableList;
   // NOTE: there may be a huge bug, because type builtins are created
   // with name generator different from type system's one.
   Int, Bool, UnitType, StringType: TType;
begin
   VarGen := TVariableGenerator.Create;
   Builtins := nil;
   Int := CreateType('Int');
   Bool := CreateType('Bool');
   UnitType := CreateType('Unit');
   StringType := CreateType('String');
   vs := VarGen.GenerateNVars(11);
   
   // built-in functions
   // pair constructor and deconstructors
   Self.Insert(Builtin('pair', BuiltinFunction('pair'), CreateFunType(vs[0], CreateFunType(vs[1], CreatePairType(vs[0], vs[1])))));
   Self.Insert(Builtin('cons', BuiltinFunction('pair'), CreateFunType(vs[2], CreateFunType(vs[3], CreatePairType(vs[2], vs[3])))));
   Self.Insert(Builtin('fst', BuiltinFunction('fst'), CreateFunType(CreatePairType(vs[3], vs[4]), vs[3])));
   Self.Insert(Builtin('snd', BuiltinFunction('snd'), CreateFunType(CreatePairType(vs[5], vs[6]), vs[6])));
   // numeric functions
   Self.Insert(Builtin('zero', BuiltinFunction('zero'), CreateFunType(Int, Bool)));
   Self.Insert(Builtin('pred', BuiltinFunction('pred'), CreateFunType(Int, Int)));
   Self.Insert(Builtin('succ', BuiltinFunction('succ'), CreateFunType(Int, Int)));
   Self.Insert(Builtin('times', BuiltinFunction('*'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('*', BuiltinFunction('*'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('plus', BuiltinFunction('+'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('+', BuiltinFunction('+'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('minus', BuiltinFunction('-'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('-', BuiltinFunction('-'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('div', BuiltinFunction('/'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('/', BuiltinFunction('/'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('mod', BuiltinFunction('%'), CreateFunType(Int, CreateFunType(Int, Int))));
   Self.Insert(Builtin('%', BuiltinFunction('%'), CreateFunType(Int, CreateFunType(Int, Int))));
   // general equality function
   Self.Insert(Builtin('eq', BuiltinFunction('eq'), CreateFunType(vs[7], CreateFunType(vs[7], Bool))));
   // NOTE: Previous line breaks compilation because in value environment keys are names and name 'eq' is allready taken
   // There are many ways to handle that:
   // * First is to create different comparison functions for different types
   // * Second is to implement fucntion overloading.
   //   (Probably through some sort of embedding function signature into environment key)
   // * Third (this was actually used) is to assign type a -> a -> Bool to this function and then
   //   use general comparison from Values unit
   // I/O functions
   Self.Insert(Builtin('println', BuiltinFunction('println'), CreateFunType(vs[8], UnitType)));
   Self.Insert(Builtin('print', BuiltinFunction('print'), CreateFunType(vs[9], UnitType)));
   Self.Insert(Builtin('read', BuiltinFunction('read'), CreateFunType(UnitType, StringType)));
   // kind of uncatchable exception
   Self.Insert(Builtin('error', BuiltinFunction('error'), CreateFunType(StringType, vs[10])));
   // string functions
   Self.Insert(Builtin('append', BuiltinFunction('append'), CreateFunType(StringType, CreateFunType(StringType, StringType))));
   Self.Insert(Builtin('length', BuiltinFunction('length'), CreateFunType(StringType, Int)));
   // these are more like generic list functions, but Goofy has no list type right now
   Self.Insert(Builtin('head', BuiltinFunction('head'), CreateFunType(StringType, StringType)));   
   Self.Insert(Builtin('tail', BuiltinFunction('tail'), CreateFunType(StringType, StringType)));
   // assertions are kinda important
   Self.Insert(Builtin('assertEquals', BuiltinFunction('assertEquals'), CreateFunType(vs[11], CreateFunType(vs[11], UnitType))));
end;

procedure TGoofyBuiltins.Insert(b: TGoofyBuiltin);
var
   len: Integer;
begin
   len := Length(Self.Builtins);
   SetLength(Self.Builtins, len + 1);
   Self.Builtins[len] := b;
end;

function TGoofyBuiltins.ApplyBuiltin(builtin: String;
                                     arg: TValue;
                                     env: TValueEnvironment): TValue;
var
   buffer, str, tail: String;
begin
   // dispatching by builtin name
   if (builtin = 'zero') then
      // no need to check for arg to have integer type
      // or is it?
      Result := BooleanV((arg as TIntegerValue).Value = 0)
   else if (builtin = 'succ') then
      Result := IntegerV((arg as TIntegerValue).Value + 1)
   else if (builtin = 'pred') then
      Result := IntegerV((arg as TIntegerValue).Value - 1)
   else if (builtin = 'fst') then
      Result := (arg as TPairValue).Fst
   else if (builtin = 'snd') then
      Result := (arg as TPairValue).Snd
   else if (builtin = 'pair') then
      Result := PABuiltinFunction('pair', arg)
   else if (builtin = '*') then
      Result := PABuiltinFunction('*', arg)
   else if (builtin = '+') then
      Result := PABuiltinFunction('+', arg)
   else if (builtin = '-') then
      Result := PABuiltinFunction('-', arg)
   else if (builtin = '/') then
      Result := PABuiltinFunction('/', arg)
   else if (builtin = '%') then
      Result := PABuiltinFunction('%', arg)
   else if (builtin = 'println') then
   begin
      writeln(PrintishValue(arg));
      Result := UnitV;
   end
   else if (builtin = 'print') then
   begin
      write(PrintishValue(arg));
      Result := UnitV;
   end
   else if (builtin = 'error') then
      raise EEvaluationStoppedError.Create((arg as TStringValue).Value)
   else if (builtin = 'eq') then
      Result := PABuiltinFunction('eq', arg)
   else if (builtin = 'read') then
   begin
      readln(buffer);
      Result := StringV(buffer);
   end
   else if (builtin = 'append') then
      Result := PABuiltinFunction('append', arg)
   else if (builtin = 'length') then
      Result := IntegerV(Length((arg as TStringValue).Value)) 
   else if (builtin = 'head') then
      Result := StringV((arg as TStringValue).Value[1])
   else if (builtin = 'tail') then
   begin
      str := (arg as TStringValue).Value;
      tail := System.Copy(str, 2, Length(str) - 1);
      Result := StringV(tail);
   end
   else if (builtin = 'assertEquals') then
      Result := PABuiltinFunction('assertEquals', arg)
   else
      raise EBuiltinError.Create('Built-in function ''' + builtin + ''' is not implemented yet');
end;

function TGoofyBuiltins.ApplyPABuiltin(builtin: String;
                                       oldarg: TValue;
                                       arg: TValue;
                                       env: TValueEnvironment): TValue;
begin
   if (builtin = 'pair') then
      Result := PairV(oldarg, arg)
   else if (builtin = '*') then
      Result := IntegerV((oldarg as TIntegerValue).Value * (arg as TIntegerValue).Value)
   else if (builtin = '+') then
      Result := IntegerV((oldarg as TIntegerValue).Value + (arg as TIntegerValue).Value)
   else if (builtin = '-') then
      Result := IntegerV((oldarg as TIntegerValue).Value - (arg as TIntegerValue).Value)
   else if (builtin = '/') then
      Result := IntegerV((oldarg as TIntegerValue).Value div (arg as TIntegerValue).Value)
   else if (builtin = '%') then
      Result := IntegerV((oldarg as TIntegerValue).Value mod (arg as TIntegerValue).Value)
   else if (builtin = 'eq') then
      Result := BooleanV(EqualValues(oldarg, arg))
   else if (builtin = 'append') then
      Result := StringV((oldarg as TStringValue).Value + (arg as TStringValue).Value)
   else if (builtin = 'assertEquals') then
   begin
      if EqualValues(oldarg, arg) then
         Result := UnitV
      else
         raise EEvaluationStoppedError.Create('Assertion failed: ' + oldarg.ToStr + ' /= ' + arg.ToStr)
   end
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
