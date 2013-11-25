program Goofy;
{$I+}
uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Classes, SysUtils, CustApp,
   expr in 'parser\expr.pas',
   AST, Parser,
   HMTypes, GoofyTypeSystem,
   Evaluator,
   Builtins,
   Repl;

type

   { TMyApplication }

   TMyApplication = class(TCustomApplication)
   protected
      Verbose: Boolean;
      procedure DoRun; override;
      procedure InterpretPath(path: String);
      procedure GoofyRepl;
   public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure WriteHelp; virtual;
   end;

   { TMyApplication }

   procedure TMyApplication.DoRun;
   var
      ErrorMsg, path: String;
      fileList: TStringList;
   begin
      // parse parameters
      fileList := TStringList.Create;
      ErrorMsg:=CheckOptions('hvr', nil, nil, fileList);
      if ErrorMsg<>'' then begin
         ShowException(Exception.Create(ErrorMsg));
         Terminate;
         Exit;
      end;
      if HasOption('h','help') then
      begin
         WriteHelp;
         Terminate;
         Exit;
      end;
      if HasOption('r','repl') then
      begin
         GoofyRepl;
         Terminate;
         Exit;
      end;
      if HasOption('v','verbose') then
      begin
         path := GetOptionValue('v', 'verbose');
         Self.Verbose := True;
      end;
      
      if (fileList.Count > 0) then
         path := fileList[0]
      else if (path <> '') then
         //pass
      else
      begin
         writeln('No files specified');
         Terminate;
         Exit;
      end;
      
      InterpretPath(path);
      
      // stop program loop
      Terminate;
   end;
   
   procedure TMyApplication.InterpretPath(path: String);
   var
      ast: TExpression;
      typeSystem: TGoofyTypeSystem;
      eval: TEvaluator;
      builtins: TGoofyBuiltins;
   begin
      try
         // parsing
         ast := ParseFile(path);
         // typechecking
         builtins := TGoofyBuiltins.Create;
         typeSystem := TGoofyTypeSystem.Create(builtins);
         typeSystem.GetExprType(ast);
         // evaluating
         eval := TEvaluator.Create(builtins);
         eval.Evaluate(ast);
      except
         on e: EFOpenError do begin
            writeln;
            writeln('Cannot find file ', path);
         end;
         on e: ETypeError do
         begin
            writeln;
            writeln('Typecheck error: ', e.Message);
         end;
         on e: EEvalError do
         begin
            writeln;
            writeln('Evaluation error: ', e.Message);
         end;
         on e: EBuiltinError do
         begin
            writeln;
            writeln('Builtin error" ', e.Message);
         end;
      end;
   end;
   
   procedure TMyApplication.GoofyRepl;
   var
      repl: TGoofyRepl;
   begin
      repl := TGoofyRepl.Create;
      repl.RunRepl;
   end;
   
   constructor TMyApplication.Create(TheOwner: TComponent);
   begin
      inherited Create(TheOwner);
      StopOnException := True;
      Self.Verbose := False;
   end;

   destructor TMyApplication.Destroy;
   begin
      inherited Destroy;
   end;

   procedure TMyApplication.WriteHelp;
   begin
      { add your help code here }
      writeln('Usage: ',ExeName,' [-h] [-v] <source file>');
   end;

var
   Application: TMyApplication;
begin
   Application:=TMyApplication.Create(nil);
   Application.Title:='Typechecker';
   Application.Run;
   Application.Free;
end.

