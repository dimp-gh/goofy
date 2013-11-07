program Goofy;

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Classes, SysUtils, CustApp,
   AST, HMTypes, HMDataStructures, HindleyMilner, GoofyTypeSystem,
   Tokenizer, Parser;

type

   { TMyApplication }

   TMyApplication = class(TCustomApplication)
   protected
      Verbose: Boolean;
      procedure DoRun; override;
      procedure Interpret(path: String);
   public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure WriteHelp; virtual;
   end;

   { TMyApplication }

   procedure TMyApplication.DoRun;
   var
      ErrorMsg: String;
      fileList: TStringList;
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
      if HasOption('v','verbose') then
         Self.Verbose := True;
      
      fileList := TStringList.Create;
      CheckOptions('', nil, nil, fileList);
      if fileList.Count = 0 then
      begin
         WriteHelp;
         Terminate;
         Exit;
      end;
      Interpret(fileList[0]);
      
      // stop program loop
      Terminate;
   end;

   procedure TMyApplication.Interpret(path: String);
   var
      tokens: TTokenList;
      ast: TExpression;
   begin
      try
         tokens := TokenizeFile(path);
         if Self.Verbose then
            PrintTokenList(tokens);
         ReportTokenizeErrors(path, tokens);
         ast := Parse(tokens);
         writeln('Parsed AST: ', ast.ToStr);
      except
         on e: ETokenizeError do
            writeln(e.Message);
         on e: EParseError do
            writeln(e.Message);
      end;
   end;
   
   constructor TMyApplication.Create(TheOwner: TComponent);
   begin
      inherited Create(TheOwner);
      StopOnException := True;
      Self.Verbose := True;
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

