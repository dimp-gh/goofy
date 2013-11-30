unit Repl;
{$mode objfpc}{$H+}
interface

uses
   Classes,
   StrUtils,
   SysUtils,
   AST,
   expr in 'parser\expr.pas',
   Parser,
   HMTypes,
   Values,
   Builtins,
   Executor;

type
   EInputError = class(Exception);
   TGoofyRepl = class
   private
      Verbose: Boolean;
      Builtins: TGoofyBuiltins;
      Exec: TGoofyExecutor;
      function ReadUserInput: String;
      function IsCommandHelp(s: String): Boolean;
      function IsCommandGetType(s: String): Boolean;
      function IsCommandShow(s: String): Boolean;
      function IsCommandQuit(s: String): Boolean;
      function IsCommandMan(s: String): Boolean;
      function IsCommandBuiltins(s: String): Boolean;
      function IsCommandLoad(s: String): Boolean;
      procedure CutCommand(var input  : String);
   public
      constructor Create;
      procedure RunRepl;
      procedure PrintHelp;
      procedure PrintMan;
      procedure PrintBuiltins;
      procedure LoadPrelude;
   end;
   
implementation

constructor TGoofyRepl.Create;
begin
   Builtins := TGoofyBuiltins.Create;
   Exec := TGoofyExecutor.Create(Builtins);
   Verbose := False;
end;

function TGoofyRepl.IsCommandHelp(s: String): Boolean;
begin
   Result := (s = ':help') or (s = ':h');
end;

function TGoofyRepl.IsCommandGetType(s: String): Boolean;
begin
   Result := AnsiStartsStr(':type ', s) or AnsiStartsStr(':t ', s);
end;

function TGoofyRepl.IsCommandShow(s: String): Boolean;
begin
   Result := AnsiStartsStr(':show ', s) or AnsiStartsStr(':s ', s);
end;

function TGoofyRepl.IsCommandQuit(s: String): Boolean;      
begin
   Result := (s = ':quit') or (s = ':q');
end;

function TGoofyRepl.IsCommandMan(s: String): Boolean;      
begin
   Result := (s = ':man');
end;

function TGoofyRepl.IsCommandBuiltins(s: String): Boolean;      
begin
   Result := (s = ':builtins');
end;

function TGoofyRepl.IsCommandLoad(s: String): Boolean;      
begin
   Result := AnsiStartsStr(':load', s) or AnsiStartsStr(':l', s);
end;

procedure TGoofyRepl.PrintHelp;
begin
   writeln('The followind commands are available:');
   writeln('  <expression>                     typecheck and evaluate given expression');
   writeln('  <statement>                      typecheck and evaluate given statement');
   writeln('  :load <module name>              load given module from current dir');
   writeln('  :type <expr>, :t <expr>          infer type for given expression');
   writeln('  :show <expr>, :s <expr>          parse and prettyprint given expression');
   writeln('  :help, :h                        print that message');
   writeln('  :man                             print small overview of language features');
   writeln('  :builtins                        print all built-in values and their signatures');
   writeln('  :quit, :q                        quit REPL');
end;

procedure TGoofyRepl.PrintMan;
begin
   writeln('Goofy is a small, statically-typed language with type inference.');
   writeln('');
end;

procedure TGoofyRepl.PrintBuiltins;
begin
   Self.Builtins.PrintBuiltins;
end;

procedure TGoofyRepl.CutCommand(var input: String);
var
   i: Integer;
begin
   i := Pos(' ', input);
   input := System.Copy(input, i + 1, Length(input) - i);
end;

procedure TGoofyRepl.RunRepl;
const
   resName = 'it';
var
   ast: TAST;
   expr: TExpression;
   stmt: TStatement;
   exprType: TType;
   value: TValue;
   input: String;
   module: TModule;
begin
   write('Loading Prelude...');
   Self.LoadPrelude;
   writeln('done');
   writeln('Welcome to Goofy REPL.');
   writeln('Enter :help to list available commands.');
   while True do
      try
         input := Self.ReadUserInput;
         if (input = '') then
            // do nothing
         else if IsCommandQuit(input) then
            break
         else if IsCommandHelp(input) then
            PrintHelp
         else if IsCommandMan(input) then
            PrintMan
         else if IsCommandBuiltins(input) then
            PrintBuiltins
         else if IsCommandGetType(input) then       
         begin
            CutCommand(input);
            // parsing
            ast := ParseString(input);
            // typechecking
            if not(ast is TExpression) then
               raise EInputError.Create('Command :type expects expression, not statement');
            expr := ast as TExpression;
            Exec.TypeSystem.ResetGeneratorNames;
            exprType := Exec.Typecheck(expr);
            write(input);
            writeln(' :: ', exprType.ToStr);
         end
         else if IsCommandShow(input) then
         begin
            CutCommand(input);
            ast := ParseString(input);
            writeln(ast.ToStr);
         end
         else if IsCommandLoad(input) then
         begin
            CutCommand(input);
            module := ParseModule('./' + input + '.gf');
            Exec.LoadModule(module);
            writeln('Loaded ', module.ToStr);
         end
         else // command is an expression or statement
         begin
            ast := ParseString(input);
            if (ast is TExpression) then
            begin
               expr := ast as TExpression;
               stmt := ValueDecl(resName, expr);
               Exec.TypeSystem.ResetGeneratorNames;
               exprType := Exec.Typecheck(expr);
               // evaluating
               Exec.Execute(stmt);
               value := Exec.GetValue(resName);
               // printing results
               write(resName, ' :: ', exprType.ToStr);
               writeln(' ~> ', value.ToStr);
            end
            else if (ast is TStatement) then
            begin
               stmt := ast as TStatement;
               Exec.Execute(stmt);
            end
            else
               Raise EInputError.Create('Parsed AST is neither expression nor statement');
         end;
      except
         on e: EInputError do
            writeln('User input error: ', e.Message);
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

function TGoofyRepl.ReadUserInput: String;
var
   code: String;
   line: String;
begin
   code := '';
   Write('> ');
   while True do
   begin
      Readln(line);
      if (line <> '') and (line[Length(line)] = '\') then
      begin
         line[Length(line)] := ' ';
         code := code + line;
      end
      else
      begin
         code := code + line;
         Break;
      end;
   end;
   Result := code;
end;

procedure TGoofyRepl.LoadPrelude;
var
   prelude: TModule;
begin
   try
      prelude := ParseModule('./Prelude.gf');
      Self.Exec.LoadModule(prelude);
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

initialization
   
end.
