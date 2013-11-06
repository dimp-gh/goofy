unit GoofyTypeSystemTests;
{$ASSERTIONS ON}
interface

uses AST, HMTypes, GoofyTypeSystem;

implementation

procedure FactorialInferTest;
var
   fact:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin
   fact := Letrec('factorial', // letrec factorial =
                  Lambda('n',    // fn n =>
                         Apply(
                            Apply(   // cond (zero n) 1
                               Apply(Ident('cond'),     // cond (zero n)
                                     Apply(Ident('zero'), Ident('n'))),
                               Ident('1')),
                            Apply(    // times n
                               Apply(Ident('times'), Ident('n')),
                               Apply(Ident('factorial'),
                                     Apply(Ident('pred'), Ident('n')))
                                 )
                              )
                        ),      // in
                  Apply(Ident('factorial'), Ident('5'))
                 );
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(fact) = 'int');
end;

procedure TypeMismatchTest;
var
   pairfun:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin
   // fn x => (pair(x(3) (x(true))))
   pairfun := Lambda('x',
                  Apply(
                     Apply(Ident('pair'),
                           Apply(Ident('x'), Ident('3'))),
                     Apply(Ident('x'), Ident('true'))));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(pairfun) = 'Type error: Type mismatch: bool /= int');      
end;

procedure UndefinedSymbolTest;
var
   pair:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin
   // fn x => (pair(x(3) (x(true))))
   pair := Apply(
      Apply(Ident('pair'), Apply(Ident('f'), Ident('4'))), 
      Apply(Ident('f'), Ident('true')));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(pair) = 'Parse error: Undefined symbol f');      
end;

procedure PairTypeTest;
var
   pair:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin
   // letrec f = (fn x => x) in ((pair (f 4)) (f true))
   pair := Let('f', Lambda('x', Ident('x')),
               Apply(
                  Apply(Ident('pair'),
                        Apply(Ident('f'), Ident('4'))),
                  Apply(Ident('f'), Ident('true'))));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(pair) = '(int * bool)');      
end;
         
procedure RecursiveUnificationTest;
var
   ast:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin
   // fn f => f f (fail)
   ast := Lambda('f', Apply(Ident('f'), Ident('f')));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(ast) = 'Type error: Recursive unification');      
end;

procedure TrickyFunctionTest;
var
   ast:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin
   // let g = fn f => 5 in g g
   ast := Let('g',
                   Lambda('f', Ident('5')),
                   Apply(Ident('g'), Ident('g')));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(ast) = 'int');      
end;

procedure GenericsTest;
var
   ast:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin
   // example that demonstrates generic and non-generic variables:
   // fn g => let f = fn x => g in pair (f 3, f true)
   ast := Lambda('g',
                 Let('f',
                     Lambda('x', Ident('g')),
                     Apply(
                        Apply(Ident('pair'),
                              Apply(Ident('f'),
                                    Ident('3'))),
                        Apply(Ident('f'),
                              Ident('true')))));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(ast) = '(a -> (a * a))');      
end;

procedure FunCompositionTest;
var
   ast:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin      
   // Function composition
   // fn f (fn g (fn arg (f g arg)))
   ast := Lambda('f', Lambda('g', Lambda('arg', Apply(Ident('g'), Apply(Ident('f'), Ident('arg'))))));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(ast) = '((a -> b) -> ((b -> c) -> (a -> c)))');      
end;

procedure IdentityTypeTest;
var
   ast:  TSyntaxNode;
   goofyTS: TGoofyTypeSystem;
begin      
   // identity function
   ast := Lambda('x', Ident('x'));
   goofyTS := TGoofyTypeSystem.Create;
   Assert(goofyTS.GetExprTypeStr(ast) = '(a -> a)');      
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
end.
