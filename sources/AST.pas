unit AST;
{$mode objfpc}{$H+}
interface

uses
   Sysutils;

type
   TAST = class abstract
      function ToStr: String; virtual; abstract;
   end;
   
   TExpression = class(TAST);

   TIdentifier = class(TExpression)
   public
      Name: String;
      function ToStr: String; override;
      constructor Create(n: String);
   end;
   
   TLiteral = class(TExpression)
   end;
   
   TIntegerLiteral = class(TLiteral)
   public
      Value: Integer;
      function ToStr: String; override;
      constructor Create(v: String);
   end;
   
   TBooleanLiteral = class(TLiteral)
   public
      Value: Boolean;
      function ToStr: String; override;
      constructor Create(v: Boolean);
   end;
   
   TUnitLiteral = class(TLiteral)
   public
      function ToStr: String; override;
      constructor Create;
   end;
   
   TPairLiteral = class(TExpression) // shouldn't it inherit from TLiteral?
   public
      Fst, Snd: TExpression;
      function ToStr: String; override;
      constructor Create(v1, v2: TExpression);
   end;
   
   TIfThenElse = class(TExpression)
   public
      Cond: TExpression;
      Then_: TExpression;
      Else_: TExpression;
      function ToStr: String; override;
      constructor Create(c, t, e: TExpression);
   end;
   
   TClause = class
   public
      Pattern: TExpression;
      Then_: TExpression;
      function ToStr: String;
      constructor Create(p, t: TExpression);      
   end;
   
   TClauseList = array of TClause;
   
   TCaseOf = class(TExpression)
   public
      Expr: TExpression;
      Clauses: TClauseList;
      function ToStr: String; override;
      constructor Create(e: TExpression; cs: TClauseList);
   end;
   
   TLambda = class(TExpression)
   public
      Variable: String;
      Body: TExpression;
      function ToStr: String; override;
      constructor Create(v: String; b: TExpression);
   end;

   TApply = class(TExpression)
   public
      Fun: TExpression;
      Argument: TExpression;
      function ToStr: String; override;
      constructor Create(fn: TExpression; arg: TExpression);
   end;

   TLet = class(TExpression)
   public
      Variable: String;
      Definition: TExpression;
      Body: TExpression;
      function ToStr: String; override;
      constructor Create(v: String; defn: TExpression; b: TExpression);
   end;

   TLetRec = class(TExpression)
   public
      Variable: String;
      Definition: TLambda;
      Body: TExpression;
      function ToStr: String; override;
      constructor Create(v: String; defn: TLambda; b: TExpression);
   end;
   
   TStatement = class(TAST);
   
   TValueDeclaration = class(TStatement)
   public
      Name: String;
      Expr: TExpression;
      function ToStr: String; override;
      constructor Create(n: String; e: TExpression);
   end;
   
   TStatementList = array of TStatement;
   
   TModule = class(TAST)
   public
      Name: String;
      Stmts: TStatementList;
      function ToStr: String; override;
      constructor Create(n: String; ss: TStatementList);
   end;
   
// expressions   
function Identifier(n: String): TIdentifier;
function IntegerLiteral(v: Integer): TIntegerLiteral;
function IntegerLiteral(v: String): TIntegerLiteral;
function BooleanLiteral(v: Boolean): TBooleanLiteral;
function PairLiteral(v1, v2: TExpression): TPairLiteral;
function UnitLiteral: TUnitLiteral;
function IfThenElse(c, t, e: TExpression): TIfThenElse;
function Lambda(v: String; b: TExpression): TLambda;
function Apply(fn: TExpression; arg: TExpression): TApply;
function Let(v: String; defn: TExpression; b: TExpression): TLet;
function LetRec(v: String; defn: TLambda; b: TExpression): TLetRec;
function CaseOf(x: TExpression; cs: TClauseList): TCaseOf;
function Clause(p, t: TExpression): TClause;
// statements
function FunctionDecl(name: String; cs: TClauseList): TValueDeclaration;
function ValueDecl(n: String; e: TExpression): TValueDeclaration;
// modules
function Module(name: String; ss: TStatementList): TModule;

implementation

constructor TIntegerLiteral.Create(v: String);
begin
   Self.Value := StrToInt(v);
   inherited Create;
end;

function TIntegerLiteral.ToStr: String;
begin
   Result := IntToStr(Self.Value);
end;

constructor TBooleanLiteral.Create(v: Boolean);
begin
   Self.Value := v;
   inherited Create;
end;

function TBooleanLiteral.ToStr: String;
begin
   if Self.Value then
      Result := 'true'
   else
      Result := 'false';
end;

function TUnitLiteral.ToStr: String;
begin
   Result := '()';
end;

function TPairLiteral.ToStr: String;
begin
   Result := '(' + Self.Fst.ToStr + ', ' + Self.Snd.ToStr + ')';
end;

constructor TPairLiteral.Create(v1, v2: TExpression);
begin
   Self.Fst := v1;
   Self.Snd := v2;
end;

constructor TUnitLiteral.Create;
begin
   inherited Create;
end;

constructor TIdentifier.Create(n: String);
begin
   Self.Name := n;
   inherited Create;
end;

function TIdentifier.ToStr: String;
begin
   Result := Self.Name;
end;

function TIfThenElse.ToStr: String;
begin
   Result := '(if ' + Self.Cond.ToStr + ' then ' + Self.Then_.ToStr + ' else ' + Self.Else_.ToStr + ')';
end;

constructor TIfThenElse.Create(c, t, e: TExpression);
begin
   inherited Create;
   Self.Cond := c;
   Self.Then_ := t;
   Self.Else_ := e;
end;

function TCaseOf.ToStr: String;
var
   i: Integer;
begin
   Result := '(case ' + Expr.ToStr + ' of' + #13#10;
   for i := 0 to High(Clauses) - 1 do
      Result := Result + '  ' + Clauses[i].ToStr + ';' + #13#10;
   Result := Result + '  ' + Clauses[High(Clauses)].ToStr + #13#10;
   Result := Result + 'end)';
end;

constructor TCaseOf.Create(e: TExpression; cs: TClauseList);
var
   i: Integer;
begin
   Self.Expr := e;
   SetLength(Self.Clauses, Length(cs));
   for i := 0 to High(cs) do
      Self.Clauses[i] := cs[i];
end;

function TClause.ToStr: String;
begin
   Result := Self.Pattern.ToStr + ' -> ' + Self.Then_.ToStr;
end;

constructor TClause.Create(p, t: TExpression);      
begin
   Self.Pattern := p;
   Self.Then_ := t;
end;

constructor TLambda.Create(v: String; b: TExpression);
begin
   Self.Variable := v;
   Self.Body := b;
   inherited Create;
end;

function TLambda.ToStr: String;
begin
   Result := '(fn ' + Self.Variable + ' => ' + Self.Body.ToStr + ')';
end;

constructor TApply.Create(fn, arg: TExpression);
begin
   Self.Fun := fn;
   Self.Argument := arg;
   inherited Create;
end;

function TApply.ToStr: String;
begin
   Result := '(' + Self.Fun.ToStr + ' ' + Self.Argument.ToStr + ')';
end;

constructor TLet.Create(v: String; defn, b: TExpression);
begin
   Self.Variable := v;
   Self.Definition := defn;
   Self.Body := b;
   inherited Create;
end;

function TLet.ToStr: String;
begin
   Result := '(' + 'let ' + Self.Variable + ' = ' + Self.Definition.ToStr + ' in ' + Self.Body.ToStr + ')';
end;

constructor TLetRec.Create(v: String; defn: TLambda; b: TExpression);
begin
   Self.Variable := v;
   Self.Definition := defn;
   Self.Body := b;
   inherited Create;
end;

function TLetRec.ToStr: String;
begin
   Result := '(' + 'letrec ' + Self.Variable + ' = ' + Self.Definition.ToStr + ' in ' + Self.Body.ToStr + ')';
end;

function TValueDeclaration.ToStr: String;
begin
   Result := 'val ' + Self.Name + ' = ' + Self.Expr.ToStr;
end;

constructor TValueDeclaration.Create(n: String; e: TExpression);
begin
   Self.Name := n;
   Self.Expr := e;
end;

function TModule.ToStr: String;
begin
   Result := '<module ' + Name + ' with ' + IntToStr(Length(Stmts)) + ' definitions>';
end;

constructor TModule.Create(n: String; ss: TStatementList);
begin
   Self.Name := n;
   Self.Stmts := ss;
end;

// A few convenient functions for creating AST
function Identifier(n: String): TIdentifier;
begin
   Result := TIdentifier.Create(n);
end;

function IntegerLiteral(v: Integer): TIntegerLiteral;
begin
   Result := TIntegerLiteral.Create(IntToStr(v));
end;

function IntegerLiteral(v: String): TIntegerLiteral;
begin
   Result := TIntegerLiteral.Create(v);
end;

function UnitLiteral: TUnitLiteral;
begin
   Result := TUnitLiteral.Create;
end;

function BooleanLiteral(v: Boolean): TBooleanLiteral;
begin
   Result := TBooleanLiteral.Create(v);
end;

function PairLiteral(v1, v2: TExpression): TPairLiteral;
begin
   Result := TPairLiteral.Create(v1, v2);
end;

function IfThenElse(c, t, e: TExpression): TIfThenElse;
begin
   Result := TIfThenElse.Create(c, t, e);
end;

function Lambda(v: String; b: TExpression): TLambda;
begin
   Result := TLambda.Create(v, b);
end;

function Apply(fn: TExpression; arg: TExpression): TApply;
begin
   Result := TApply.Create(fn, arg);
end;

function Let(v: String; defn: TExpression; b: TExpression): TLet;
begin
   Result := TLet.Create(v, defn, b);
end;

function LetRec(v: String; defn: TLambda; b: TExpression): TLetRec;
begin
   Result := TLetRec.Create(v, defn, b);
end;

function CaseOf(x: TExpression; cs: TClauseList): TCaseOf;
begin
   Result := TCaseOf.Create(x, cs);
end;

function Clause(p, t: TExpression): TClause;
begin
   Result := TClause.Create(p, t);
end;

function FunctionDecl(name: String; cs: TClauseList): TValueDeclaration;
begin
   Result := ValueDecl(name,
                       LetRec(name,
                              Lambda('<some id>',
                                     CaseOf(Identifier('<some id>'),
                                            cs)),
                              Identifier(name)));
end;

function ValueDecl(n: String; e: TExpression): TValueDeclaration;
begin
   Result := TValueDeclaration.Create(n, e);
end;

function Module(name: String; ss: TStatementList): TModule;
begin
   Result := TModule.Create(name, ss);
end;

initialization

end.
