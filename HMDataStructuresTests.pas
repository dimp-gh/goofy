unit HMDataStructuresTests;
{$ASSERTIONS ON}
interface

uses HMTypes, HMDataStructures;

implementation

procedure EnvironmentInsertTest;
var
   env: TEnvironment;
   bool: TType;
begin
   bool := TParameterizedType.Create('bool', []);
   env := EnvNew;
   env := EnvInsert(env, 'true', bool);
   Assert(EnvFind(env, 'true'));
end;

procedure EnvironmentLookupTest;
var
   env: TEnvironment;
   bool: TType;
begin
   bool := TParameterizedType.Create('bool', []);
   env := EnvNew;
   env := EnvInsert(env, 'true', bool);
   Assert(EnvLookup(env, 'true').ToStr = 'bool');
end;

procedure EnvironmentDeleteTest;
var
   env: TEnvironment;
   bool: TType;
begin
   bool := TParameterizedType.Create('bool', []);
   env := EnvNew;
   env := EnvInsert(env, 'true', bool);
   Assert(EnvFind(env, 'true'));
   env := EnvDelete(env, 'true');
   Assert(not EnvFind(env, 'true'));
end;

procedure VarListInsertTest;
var
   list: TTypeVariableList;
   gen: TNameGenerator;
   v: TTypeVariable;
begin
   gen := TNameGenerator.Create;
   v := TTypeVariable.Create(0, @gen);
   list := VarListNew;
   list := VarListInsert(list, v);
   Assert(VarListFind(list, v));
end;

procedure VarListDeleteTest;
var
   list: TTypeVariableList;
   gen: TNameGenerator;
   v: TTypeVariable;
begin
   gen := TNameGenerator.Create;
   v := TTypeVariable.Create(0, @gen);
   list := VarListNew;
   list := VarListInsert(list, v);
   Assert(VarListFind(list, v));
   list := VarListDelete(list, v);
   Assert(not VarListFind(list, v));
end;

initialization
   EnvironmentInsertTest;
   EnvironmentLookupTest;
   EnvironmentDeleteTest;
   VarListInsertTest;
   VarListDeleteTest;
end.
