unit GoofyTypeSystemTests;
{$ASSERTIONS ON}
interface

uses AST, HMTypes, GoofyTypeSystem, Builtins;

implementation

procedure FactorialInferTest;
var
   fact:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   fact := Letrec('fact', // letrec factorial =
                  Lambda('n',    // fn n =>
                         Apply(
                            Apply(   // cond (zero n) 1
                               Apply(Ident('cond'),     // cond (zero n)
                                     Apply(Ident('zero'), Ident('n'))),
                               IntegerLiteral(1)),
                            Apply(    // times n
                               Apply(Ident('times'), Ident('n')),
                               Apply(Ident('fact'),
                                     Apply(Ident('pred'), Ident('n')))
                                 )
                              )
                        ),      // in
                  Apply(Ident('fact'), IntegerLiteral(5))
                 );
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(fact) = 'int');
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
                     Apply(Ident('pair'),
                           Apply(Ident('x'), IntegerLiteral(3))),
                     Apply(Ident('x'), Ident('true'))));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(pairfun) = 'Type error: Type mismatch: bool /= int');      
end;

procedure UndefinedSymbolTest;
var
   pair:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // fn x => (pair(x(3) (x(true))))
   pair := Apply(
      Apply(Ident('pair'), Apply(Ident('f'), Ident('4'))), 
      Apply(Ident('f'), Ident('true')));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(pair) = 'Parse error: Undefined symbol f');      
end;

procedure PairTypeTest;
var
   pair:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // letrec f = (fn x => x) in ((pair (f 4)) (f true))
   pair := Let('f', Lambda('x', Ident('x')),
               Apply(
                  Apply(Ident('pair'),
                        Apply(Ident('f'), IntegerLiteral(4))),
                  Apply(Ident('f'), Ident('true'))));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(pair) = '(int * bool)');      
end;
         
procedure RecursiveUnificationTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin
   // fn f => f f (fail)
   ast := Lambda('f', Apply(Ident('f'), Ident('f')));
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
                   Apply(Ident('g'), Ident('g')));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(ast) = 'int');      
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
                     Lambda('x', Ident('g')),
                     Apply(
                        Apply(Ident('pair'),
                              Apply(Ident('f'),
                                    IntegerLiteral(3))),
                        Apply(Ident('f'),
                              Ident('true')))));
   bs := TGoofyBuiltins.Create;
   goofyTS := TGoofyTypeSystem.Create(bs);
   Assert(goofyTS.GetExprTypeStr(ast) = '(a -> (a * a))');      
end;

procedure FunCompositionTest;
var
   ast:  TExpression;
   goofyTS: TGoofyTypeSystem;
   bs: TGoofyBuiltins;
begin      
   // Function composition
   // fn f (fn g (fn arg (f g arg)))
   ast := Lambda('f', Lambda('g', Lambda('arg', Apply(Ident('g'), Apply(Ident('f'), Ident('arg'))))));
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
   ast := Lambda('x', Ident('x'));
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
   Assert(goofyTS.GetExprTypeStr(ast) = 'Type error: Type mismatch: (int -> a) /= int');
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
