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
      ast: TSyntaxNode;
   begin
      goofyTS := TGoofyTypeSystem.Create;
      ast := Lambda('f',
                    Lambda('g',
                           Lambda('arg',
                                  Apply(Ident('g'),
                                        Apply(Ident('f'),
                                              Ident('arg')))))); // infers bad type
      writeln(ast.ToStr, ' :: ', goofyTS.GetExprTypeStr(ast));
      ast := Lambda('x', Ident('x')); // infers bad type (a -> b)
      writeln(ast.ToStr, ' :: ', goofyTS.GetExprTypeStr(ast));
      ast := Lambda('f', Apply(Ident('f'), Ident('f')));
      writeln(ast.ToStr, ' :: ', goofyTS.GetExprTypeStr(ast));
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

