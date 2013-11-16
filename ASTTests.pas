unit AstTests;
{$ASSERTIONS ON}
interface

uses AST;

implementation

procedure ASTPrettyPrintingTest;
var
   ast: TExpression;
begin
   ast := Let('f', Lambda('x', Ident('x')), Apply(Ident('f'), IntegerLiteral(5)));
   Assert(ast.ToStr = '(let f = (fn x => x) in (f 5))')
end;

initialization
   ASTPrettyPrintingTest;
end.
