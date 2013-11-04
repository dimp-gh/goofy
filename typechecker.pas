program typechecker;

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Classes, SysUtils, CustApp,
   AST, HMTypes, HindleyMilner, GoofyTypeSystem;

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
      // ast := TLambda.Create('f',
      //                       TLambda.Create('g',
      //                                      TLambda.Create('arg',
      //                                                     TApply.Create(TIdent.Create('g'),
      //                                                                   TApply.Create(TIdent.Create('f'),
      //                                                                                 TIdent.Create('arg'))))));
      ast := TLambda.Create('x', TIdent.Create('x')); // infers bad type (a -> b)
      
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
