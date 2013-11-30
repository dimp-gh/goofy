program Goofy;
{$I+}
uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Classes, SysUtils, CustApp,
   expr in 'parser\expr.pas',
   AST, Parser,
   HMTypes,
   Builtins,
   Repl,
   Executor;

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
      builtins: TGoofyBuiltins;
      exec: TGoofyExecutor;
      prelude, module: TModule;
      main: TStatement;
      mainType: TType;
   begin
      try
         builtins := TGoofyBuiltins.Create;
         exec := TGoofyExecutor.Create(builtins);
         prelude := ParseModule('./Prelude.gf');
         exec.LoadModule(prelude);
         module := ParseModule(path);
         exec.LoadModule(module);
         exec.GetValue('main');
         mainType := exec.Typecheck(Identifier('main'));
         if (mainType is TParameterizedType) and
               ((mainType as TParameterizedType).Args[0] is TParameterizedType) and
               ((mainType as TParameterizedType).Args[1] is TParameterizedType) and
               (((mainType as TParameterizedType).Args[0] as TParameterizedType).Name = 'Unit') and
               (((mainType as TParameterizedType).Args[1] as TParameterizedType).Name = 'Unit') then
            // pass
         else
            raise EEvalError.Create('Main function has type other than (Unit -> Unit).');
         main := ValueDecl('it', Apply(Identifier('main'), UnitLiteral));
         exec.Execute(main);
      except
         on e: EExprParserException do
            writeln('Syntax error: ', e.Message);
         on e: EParseError do
            writeln('Parsing error: ', e.Message);
         on e: ETypeError do
            writeln('Typecheck error: ', e.Message);
         on e: EEvalError do
            writeln('Evaluation error: ', e.Message);
         on e: EBuiltinError do
            writeln('Builtin error: ', e.Message);
         on e: EEvaluationStoppedError do
            writeln('Evaluation halted with message "' + e.Message + '"');
         on e: EExecError do
            writeln('Execution error: ', e.Message);
         on e: Exception do
            writeln('Weird error: ', e.Message);
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

