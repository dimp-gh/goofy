unit ParserHelper;
{$mode objfpc}{$H+}
interface

uses AST, HMTypes;

function SingleClause(c: TClause): TClauseList;
function PrependClause(c: TClause; cs: TClauseList): TClauseList;
function GetClauses(decl: TValueDeclaration): TClauseList;
function SingleStmt(s: TStatement): TStatementList;
function PrependStmt(s: TStatement; ss: TStatementList): TStatementList;
function CutStringLiteral(raw: String): String;
function Unescape(raw: String): String;
function SingleType(s: TType): TTypeList;
function PrependType(s: TType; ss: TTypeList): TTypeList;
   
implementation

function SingleClause(c: TClause): TClauseList;
var
   r: TClauseList;
begin
   SetLength(r, 1);
   r[0] := c;
   Result := r;
end;

function PrependClause(c: TClause; cs: TClauseList): TClauseList;
var
   i, len: Integer;
   res: TClauseList;
begin
   len := Length(cs);
   SetLength(res, len + 1);
   res[0] := c;
   for i := 0 to High(cs) do
      res[i + 1] := cs[i];
   Result := res;
end;

function GetClauses(decl: TValueDeclaration): TClauseList;
var
   lr: TLetRec;
   l: TLambda;
   c: TCaseOf;
begin
   lr := decl.Expr as TLetRec;
   l := lr.Definition as TLambda;
   c := l.body as TCaseOf;
   Result := c.Clauses;
end;

function SingleStmt(s: TStatement): TStatementList;
var
   r: TStatementList;
begin
   SetLength(r, 1);
   r[0] := s;
   Result := r;
end;

function PrependStmt(s: TStatement; ss: TStatementList): TStatementList;
var
   i, len: Integer;
   res: TStatementList;
begin
   len := Length(ss);
   SetLength(res, len + 1);
   res[0] := s;
   for i := 0 to High(ss) do
      res[i + 1] := ss[i];
   Result := res;
end;

function CutStringLiteral(raw: String): String;
begin
   Result := System.Copy(raw, 2, Length(raw) - 2);   
end;

function Unescape(raw: String): String;
begin
   // TODO
   Result := raw;
end; 

function SingleType(s: TType): TTypeList;
var
   r: TTypeList;
begin
   SetLength(r, 1);
   r[0] := s;
   Result := r;
end;

function PrependType(s: TType; ss: TTypeList): TTypeList;
var
   i, len: Integer;
   res: TTypeList;
begin
   len := Length(ss);
   SetLength(res, len + 1);
   res[0] := s;
   for i := 0 to High(ss) do
      res[i + 1] := ss[i];
   Result := res;
end;

initialization
   
end.
