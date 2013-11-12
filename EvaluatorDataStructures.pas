unit EvaluatorDataStructures;
{$mode objfpc}{$H+}
interface

uses SysUtils, Values;

type   
   TValueEnvironmentEntry = record
      Key: String;
      Value: TValue;
   end;  
   TValueEnvironment = array of TValueEnvironmentEntry;
   
function EnvNew: TValueEnvironment;
function EnvInsert(env: TValueEnvironment; key: String; value: TValue): TValueEnvironment;
function EnvFind(env: TValueEnvironment; key: String): Boolean;
function EnvLookup(env: TValueEnvironment; key: String): TValue;
function EnvDelete(env: TValueEnvironment; key: String): TValueEnvironment;
procedure EnvPrint(env: TValueEnvironment);

implementation

function EnvNew: TValueEnvironment;
begin
   Result := Nil;
end;

function EnvInsert(env: TValueEnvironment; key: String; value: TValue): TValueEnvironment;
var
   newEnv: TValueEnvironment;
   i, len: Integer;
begin
   len := Length(env);
   if (len <> 0) then
   begin
      if EnvFind(env, key) then
         raise Exception.Create('Cannot insert an existing key:' + key);
      SetLength(newEnv, len + 1);
      for i := 0 to len - 1 do
         newEnv[i] := env[i];
      newEnv[len].Key := key;
      newEnv[len].Value := value;
   end
   else
   begin
      SetLength(newEnv, 1);
      newEnv[0].Key := key;
      newEnv[0].Value := value;
   end;
   Result := newEnv;
 end;

function EnvFind(env: TValueEnvironment; key: String): Boolean;
var
   i: Integer;
begin
   for i := 0 to High(env) do
      if env[i].Key = key then
      begin
         Result := True;
         Exit;
      end;
   Result := False;
end;

function EnvLookup(env: TValueEnvironment; key: String): TValue;
var
   i: Integer;
begin
   for i := 0 to High(env) do
      if env[i].Key = key then
      begin
         Result := env[i].Value;
         Exit;
      end;
   raise Exception.Create('Environment has no such key: ' + key + '. Use EnvFind before EnvLookup.');
end;

function EnvDelete(env: TValueEnvironment; key: String): TValueEnvironment;
var
   newEnv: TValueEnvironment;
   i, index: Integer;
begin
   SetLength(newEnv, Length(env) - 1);
   index := 0;
   for i := 0 to High(Env) do
      if env[i].Key <> key then
      begin
         newEnv[index] := env[i];
         index := index + 1;
      end;
   Result := newEnv;
end;

procedure EnvPrint(env: TValueEnvironment);
var
   i: Integer;
begin
   writeln('Environment:');
   if Length(env) = 0 then
      writeln('Empty environment')
   else
      for i := 0 to Length(env) - 1 do
         writeln(env[i].Key, ' -> ', env[i].Value.ToStr);
end;

initialization

end.
