program Goofy;
{$I+}
uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Classes, SysUtils, CustApp,
   Tokenizer,
   AST, Parser,
   HMTypes, GoofyTypeSystem,
   Values, Evaluator,
   Builtins;

type

   { TMyApplication }

   TMyApplication = class(TCustomApplication)
   protected
      Verbose: Boolean;
      procedure DoRun; override;
      procedure InterpretPath(path: String);
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
      ErrorMsg:=CheckOptions('hv', nil, nil, fileList);
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
         WriteHelp;
         Terminate;
         Exit;
      end;
      
      InterpretPath(path);
      
      // stop program loop
      Terminate;
   end;
   
   procedure TMyApplication.InterpretPath(path: String);
   var
      tokens: TTokenList;
      ast: TExpression;
      typeSystem: TGoofyTypeSystem;
      exprType: TType;
      eval: TEvaluator;
      value: TValue;
   begin
      try
         // lexing
         tokens := TokenizeFile(path);
         if Self.Verbose then
            PrintTokenList(tokens);
         ReportTokenizeErrors(path, tokens);
         // parsing
         ast := Parse(tokens);
         write(ast.ToStr);
         // typechecking
         typeSystem := TGoofyTypeSystem.Create;
         exprType := typeSystem.GetExprType(ast);
         write(' :: ', exprType.ToStr);
         // evaluating
         eval := TEvaluator.Create;
         value := eval.Evaluate(ast);
         writeln(' => ', value.ToStr);
      except
         on e: EFOpenError do
            writeln('Cannot find file ', path);
         on e: ETokenizeError do
            writeln(e.Message);
         on e: EParseError do
            writeln(e.Message);
         on e: ETypeError do
            writeln('Typecheck error: ', e.Message);
         on e: EEvalError do
            writeln('Evaluation error: ', e.Message)
      end;
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

