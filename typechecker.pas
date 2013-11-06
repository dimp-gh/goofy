program typechecker;

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Classes, SysUtils, CustApp,
   AST, HMTypes, HMDataStructures, HindleyMilner, GoofyTypeSystem;

type

   { TMyApplication }

   TMyApplication = class(TCustomApplication)
   protected
      procedure DoRun; override;
      procedure Main;
   public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure WriteHelp; virtual;
   end;

   { TMyApplication }

   procedure TMyApplication.DoRun;
   var
      ErrorMsg: String;
   begin
      // quick check parameters
      ErrorMsg:=CheckOptions('h','help');
      if ErrorMsg<>'' then begin
         ShowException(Exception.Create(ErrorMsg));
         Terminate;
         Exit;
      end;

      // parse parameters
      if HasOption('h','help') then begin
         WriteHelp;
         Terminate;
         Exit;
      end;

      { add your program here }
      Main;

      // stop program loop
      Terminate;
   end;

   procedure TMyApplication.Main;
   var
      goofyTS: TGoofyTypeSystem;
      pair: TSyntaxNode;
      examples: array of TSyntaxNode;
      i: Integer;
   begin
      goofyTS := TGoofyTypeSystem.Create;
      
      pair := Apply(Apply(Ident('pair'), Apply(Ident('f'), Ident('4'))), Apply(Ident('f'), Ident('true')));
      SetLength(examples, 9);
      // factorial
      examples[0] := Letrec('factorial', // letrec factorial =
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
      
      // Should fail:
      // fn x => (pair(x(3) (x(true))))
      examples[1] := Lambda('x',
                            Apply(
                               Apply(Ident('pair'),
                                     Apply(Ident('x'), Ident('3'))),
                               Apply(Ident('x'), Ident('true'))));
      
      // pair(f(3), f(true))
      examples[2] := Apply(
         Apply(Ident('pair'), Apply(Ident('f'), Ident('4'))), 
         Apply(Ident('f'), Ident('true')));
         
         
      // letrec f = (fn x => x) in ((pair (f 4)) (f true))
      examples[3] := Let('f', Lambda('x', Ident('x')), pair);
         
      // fn f => f f (fail)
      examples[4] := Lambda('f', Apply(Ident('f'), Ident('f')));
         
      // let g = fn f => 5 in g g
      examples[5] := Let('g',
                         Lambda('f', Ident('5')),
                         Apply(Ident('g'), Ident('g')));
         
      // example that demonstrates generic and non-generic variables:
      // fn g => let f = fn x => g in pair (f 3, f true)
      examples[6] := Lambda('g',
                            Let('f',
                                Lambda('x', Ident('g')),
                                Apply(
                                   Apply(Ident('pair'),
                                         Apply(Ident('f'), Ident('3'))
                                        ),
                                   Apply(Ident('f'), Ident('true')))));
      
      // Function composition
      // fn f (fn g (fn arg (f g arg)))
      examples[7] := Lambda('f', Lambda('g', Lambda('arg', Apply(Ident('g'), Apply(Ident('f'), Ident('arg'))))));

      examples[8] := Lambda('x', Ident('x'));
      
      for i := 0 to High(examples) do
         writeln(examples[i].ToStr, ' :: ', goofyTS.GetExprTypeStr(examples[i]));
   end;
   
   constructor TMyApplication.Create(TheOwner: TComponent);
   begin
      inherited Create(TheOwner);
      StopOnException:=True;
   end;

   destructor TMyApplication.Destroy;
   begin
      inherited Destroy;
   end;

   procedure TMyApplication.WriteHelp;
   begin
      { add your help code here }
      writeln('Usage: ',ExeName,' -h');
   end;

var
   Application: TMyApplication;
begin
   Application:=TMyApplication.Create(nil);
   Application.Title:='Typechecker';
   Application.Run;
   Application.Free;
end.

