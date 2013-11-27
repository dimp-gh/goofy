unit ParserHelper;
{$mode objfpc}{$H+}
interface

uses AST;

function SingleClause(c: TClause): TClauseList;
function PrependClause(c: TClause; cs: TClauseList): TClauseList;
function GetClauses(decl: TValueDeclaration): TClauseList;

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

initialization
   
end.
