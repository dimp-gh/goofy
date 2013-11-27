unit Repl;
{$mode objfpc}{$H+}
interface

uses
   Classes,
   StrUtils,
   SysUtils,
   expr in 'parser\expr.pas',
   AST,
   Parser,
   HMTypes,
   GoofyTypeSystem,
   Values,
   Builtins,
   Evaluator;

type
   TGoofyRepl = class
   private
      Verbose: Boolean;
      Builtins: TGoofyBuiltins;
      Eval: TEvaluator;
      TypeSystem: TGoofyTypeSystem;
      function ReadUserInput: String;
      function IsCommandHelp(s: String): Boolean;
      function IsCommandGetType(s: String): Boolean;
      function IsCommandShow(s: String): Boolean;
      function IsCommandQuit(s: String): Boolean;
      function IsCommandMan(s: String): Boolean;
      function IsCommandBuiltins(s: String): Boolean;
      procedure CutCommand(var input: String);
   public
      constructor Create;
      procedure RunRepl;
      procedure PrintHelp;
      procedure PrintMan;
      procedure PrintBuiltins;
   end;
   
implementation

constructor TGoofyRepl.Create;
begin
   Builtins := TGoofyBuiltins.Create;
   TypeSystem := TGoofyTypeSystem.Create(Builtins);
   Eval := TEvaluator.Create(Builtins);
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

procedure TGoofyRepl.PrintHelp;
begin
   writeln('The followind commands are available:');
   writeln('  <expression>                     typecheck and evaluate given expression');
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
var
   ast: TExpression;
   exprType: TType;
   value: TValue;
   input: String;
begin
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
            typeSystem.ResetGeneratorNames;
            exprType := typeSystem.GetExprType(ast);
            write(ast.ToStr);
            writeln(' :: ', exprType.ToStr);
         end
         else if IsCommandShow(input) then
         begin
            CutCommand(input);
            ast := ParseString(input);
            writeln(ast.ToStr);
         end
         else // command is an expression
         begin
            // parsing
            ast := ParseString(input);
            // typechecking
            typeSystem.ResetGeneratorNames;
            exprType := typeSystem.GetExprType(ast);
            // evaluating
            value := eval.Evaluate(ast);
            // printing results
            write(input);
            write(' :: ', exprType.ToStr);
            writeln(' => ', value.ToStr);
         end;
      except
         on e: EExprParserException do
            writeln('ExprParser error: ', e.Message);
         on e: EParseError do
            writeln('Parsing error: ', e.Message);
         on e: ETypeError do
            writeln('Typecheck error: ', e.Message);
         on e: EEvalError do
            writeln('Evaluation error: ', e.Message);
         on e: EBuiltinError do
            writeln('Builtin error: ', e.Message);
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
      if line[Length(line)] = '\' then
      begin
         line[Length(line)] := ' ';
         code := code + line;
      end
      else
      begin
         code := code + line;
         Break
      end;
   end;
   Result := code;
end;

initialization
   
end.
