unit HMDataStructures;
{$mode objfpc}{$H+}
interface

uses HMTypes, SysUtils;

type
  
   // Environment for mapping types to it's names
   TEnvironmentEntry = record
      Key: String;
      Value: TType;
   end;  
   TEnvironment = array of TEnvironmentEntry;
   
   // Variable and type-list for storing non-generic variables
   TVariableList = array of TVariable;
   TTypeList = array of TType;
   
   // Used for creating fresh type
   TVarMapEntry = record
      Key: TVariable;
      Value: TVariable;
   end;
   TVariableMap = array of TVarMapEntry;
   
function EnvNew: TEnvironment;
function EnvInsert(env: TEnvironment; key: String; value: TType): TEnvironment;
function EnvFind(env: TEnvironment; key: String): Boolean;
function EnvLookup(env: TEnvironment; key: String): TType;
function EnvDelete(env: TEnvironment; key: String): TEnvironment;
procedure EnvPrint(env: TEnvironment);

function VarListNew: TVariableList;
function VarListInsert(list: TVariableList; v: TVariable): TVariableList;
function VarListDelete(list: TVariableList; v: TVariable): TVariableList;
function VarListFind(list: TVariableList; v: TVariable): Boolean;
function VarListToTypeList(list: TVariableList): TTypeList;
procedure VarListPrint(list: TVariableList);

function VarMapNew: TVariableMap;
function VarMapInsert(map: TVariableMap; key: TVariable; value: TVariable): TVariableMap;
function VarMapFind(map: TVariableMap; key: TVariable): Boolean;
function VarMapLookup(map: TVariableMap; key: TVariable): TVariable;
function VarMapDelete(map: TVariableMap; key: TVariable): TVariableMap;
procedure VarMapPrint(map: TVariableMap);

implementation

function EnvNew: TEnvironment;
begin
   Result := Nil;
end;

function EnvInsert(env: TEnvironment; key: String; value: TType): TEnvironment;
var
   newEnv: TEnvironment;
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

function EnvFind(env: TEnvironment; key: String): Boolean;
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

function EnvLookup(env: TEnvironment; key: String): TType;
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

function EnvDelete(env: TEnvironment; key: String): TEnvironment;
var
   newEnv: TEnvironment;
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

procedure EnvPrint(env: TEnvironment);
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

function VarListNew: TVariableList;
begin
   Result := Nil;
end;

function VarListInsert(list: TVariableList; v: TVariable): TVariableList;
var
   len: Integer;
begin
   len := Length(list);
   SetLength(list, len + 1);
   list[len] := v;
   Result := list;
end;

function VarListDelete(list: TVariableList; v: TVariable): TVariableList;
var
   i, index, len: Integer;
   newList: TVariableList;
begin
   len := Length(list);
   SetLength(newList, len - 1);
   index := 0;
   for i := 0 to len - 1 do
      if list[i].Id <> v.Id then
      begin
         newList[index] := list[i];
         index := index + 1;
      end;
   Result := newList;
end;

procedure VarListPrint(list: TVariableList);
var
   i: Integer;
begin
   writeln('Variable list:');
   if Length(list) = 0 then
      writeln('List is empty')
   else
      for i := 0 to High(list) do
         writeln(list[i].ToStr);
end;

function VarListFind(list: TVariableList; v: TVariable): Boolean;
var
   i: Integer;
begin
   for i := 0 to High(list) do
      if list[i].Id = v.Id then
      begin
         Result := True;
         Exit;
      end;
   Result := False;
end;

function VarListToTypeList(list: TVariableList): TTypeList;
var
   types: TTypeList;
   i: Integer;
begin
   SetLength(types, Length(list));
   for i := 0 to High(list) do
      types[i] := list[i];
   Result := types;
end;

function VarMapNew: TVariableMap;
begin
   Result := nil;
end;

function VarMapInsert(map: TVariableMap; key: TVariable; value: TVariable): TVariableMap;
var
   newMap: TVariableMap;
   i, len: Integer;
begin
   len := Length(map);
   if (len <> 0) then
   begin
      if VarMapFind(map, key) then
         raise Exception.Create('Cannot insert an existing key:' + key.ToStr);
      SetLength(newMap, len + 1);
      for i := 0 to len - 1 do
         newMap[i] := Map[i];
      newMap[len].Key := key;
      newMap[len].Value := value;
   end
   else
   begin
      SetLength(newMap, 1);
      newMap[0].Key := key;
      newMap[0].Value := value;
   end;
   Result := newMap;
end;

function VarMapFind(map: TVariableMap; key: TVariable): Boolean;
var
   i: Integer;
begin
   for i := 0 to High(map) do
      if map[i].Key.Id = key.Id then
      begin
         Result := True;
         Exit;
      end;
   Result := False;
end;

function VarMapLookup(map: TVariableMap; key: TVariable): TVariable;
var
   i: Integer;
begin
   for i := 0 to High(map) do
      if map[i].Key.Id = key.Id then
      begin
         Result := map[i].Value;
         Exit;
      end;
   raise Exception.Create('VariableMap has no such key: ' + key.ToStr + '. Use EnvFind before EnvLookup.');
end;

function VarMapDelete(map: TVariableMap; key: TVariable): TVariableMap;
var
   newMap: TVariableMap;
   i, index: Integer;
begin
   SetLength(newMap, Length(map) - 1);
   index := 0;
   for i := 0 to High(map) do
      if map[i].Key.Id <> key.Id then
      begin
         newMap[index] := map[i];
         index := index + 1;
      end;
   Result := newMap;
end;

procedure VarMapPrint(map: TVariableMap);
var
   i: Integer;
begin
   writeln('Variable map:');
   if Length(map) = 0 then
      writeln('Empty environment')
   else
      for i := 0 to High(map) do
         writeln(map[i].Key.ToStr, ' -> ', map[i].Value.ToStr);
end;

initialization

end.
