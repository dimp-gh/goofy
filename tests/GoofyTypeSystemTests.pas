unit GoofyTypeSystemTests;
{$ASSERTIONS ON}
interface

uses AST, GoofyTypeSystem, Builtins, StrUtils;

implementation

procedure FactorialInferTest;
var
   fact:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   fact := Letrec('fact', // letrec factorial =
                  Lambda('n',    // fn n =>
                         IfThenElse(
                            Apply(Identifier('zero'), Identifier('n')),
                            IntegerLiteral(1),
                            Apply(    // times n
                               Apply(Identifier('times'), Identifier('n')),
                               Apply(Identifier('fact'),
                                     Apply(Identifier('pred'), Identifier('n')))
                                 ))),
                  Apply(Identifier('fact'), IntegerLiteral(5))
                 );
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(fact) = 'Int');
end;

procedure TypeMismatchTest;
var
   pairfun:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // fn x => (pair(x(3) (x(true))))
   pairfun := Lambda('x',
                  Apply(
                     Apply(Identifier('pair'),
                           Apply(Identifier('x'), IntegerLiteral(3))),
                     Apply(Identifier('x'), BooleanLiteral(True))));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(IsWild(goofyTS.GetExprTypeStr(pairfun),'*Bool /= Int*', False));
end;

procedure UndefinedSymbolTest;
var
   pair:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // fn x => (pair(x(3) (x(true))))
   pair := Apply(
      Apply(Identifier('pair'), Apply(Identifier('f'), Identifier('4'))), 
      Apply(Identifier('f'), BooleanLiteral(True)));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(IsWild(goofyTS.GetExprTypeStr(pair), '*Undefined symbol f*', False));      
end;

procedure PairTypeTest;
var
   pair:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // letrec f = (fn x => x) in ((pair (f 4)) (f true))
   pair := Let('f', Lambda('x', Identifier('x')),
               Apply(
                  Apply(Identifier('pair'),
                        Apply(Identifier('f'), IntegerLiteral(4))),
                  Apply(Identifier('f'), BooleanLiteral(True))));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(pair) = '(Int, Bool)');      
end;
         
procedure RecursiveUnificationTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // fn f => f f (fail)
   ast := Lambda('f', Apply(Identifier('f'), Identifier('f')));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(ast) = 'Type error: Recursive unification');      
end;

procedure TrickyFunctionTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // let g = fn f => 5 in g g
   ast := Let('g',
                   Lambda('f', IntegerLiteral(5)),
                   Apply(Identifier('g'), Identifier('g')));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(ast) = 'Int');      
end;

procedure GenericsTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // example that demonstrates generic and non-generic variables:
   // fn g => let f = fn x => g in pair (f 3, f true)
   ast := Lambda('g',
                 Let('f',
                     Lambda('x', Identifier('g')),
                     Apply(
                        Apply(Identifier('pair'),
                              Apply(Identifier('f'),
                                    IntegerLiteral(3))),
                        Apply(Identifier('f'),
                              BooleanLiteral(true)))));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(ast) = '(a -> (a, a))');      
end;

procedure FunCompositionTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin      
   // Function composition
   // fn f (fn g (fn arg (f g arg)))
   ast := Lambda('f', Lambda('g', Lambda('arg', Apply(Identifier('g'), Apply(Identifier('f'), Identifier('arg'))))));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(ast) = '((a -> b) -> ((b -> c) -> (a -> c)))');      
end;

procedure IdentityTypeTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin      
   // identity function
   ast := Lambda('x', Identifier('x'));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(ast) = '(a -> a)');      
end;

procedure ApplyNumberTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   ast := Apply(IntegerLiteral(5), IntegerLiteral(10));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(IsWild(goofyTS.GetExprTypeStr(ast),'*(Int -> a) /= Int*', False));
end;

initialization
   FactorialInferTest;
   TypeMismatchTest;
   UndefinedSymbolTest;
   PairTypeTest;
   RecursiveUnificationTest;
   TrickyFunctionTest;
   GenericsTest;
   FunCompositionTest;
   IdentityTypeTest;
   ApplyNumberTest;
end.
