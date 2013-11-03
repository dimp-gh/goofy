unit AstTests;
{$ASSERTIONS ON}
interface

uses AST;

implementation

procedure ASTPrettyPrintingTest;
var
   ast: TSyntaxNode;
begin
   ast := TLet.Create('f', TLambda.Create('x', TIdent.Create('x')), TApply.Create(TIdent.Create('f'), TIdent.Create('5')));
   Assert(ast.ToStr = '(let f = (fn x => x) in (f 5))')
end;

initialization
   ASTPrettyPrintingTest;
end.
