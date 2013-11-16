unit Repl;
{$mode objfpc}{$H+}
interface

uses
   Classes,
   AST,
   Tokenizer,
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
      function ReadUserInput: TStringList;
   public
      constructor Create;
      procedure RunRepl;
   end;
   
implementation

constructor TGoofyRepl.Create;
begin
   Builtins := TGoofyBuiltins.Create;
   TypeSystem := TGoofyTypeSystem.Create(Builtins);
   Eval := TEvaluator.Create(Builtins);
   Verbose := False;
end;

procedure TGoofyRepl.RunRepl;
var
   tokens: TTokenList;
   ast: TExpression;
   exprType: TType;
   value: TValue;
   input: TStringList;
begin
   writeln('Welcome to Goofy REPL.');
   while True do
      try
         input := Self.ReadUserInput;
         if (input <> nil) and (input[0] = ':quit') then
            break;
         // lexing
         tokens := TokenizeStringList(input);
         if Self.Verbose then
            PrintTokenList(tokens);
         ReportTokenizeErrors('<user input>', tokens);
         // parsing
         ast := Parse(tokens);
         // typechecking
         exprType := typeSystem.GetExprType(ast);
         // evaluating
         value := eval.Evaluate(ast);
         // printing results
         write(ast.ToStr);
         write(' :: ', exprType.ToStr);
         writeln(' => ', value.ToStr);
      except
         on e: ETokenizeError do
         begin
            writeln;
            writeln(e.Message);
         end;
         on e: EParseError do
         begin
            writeln;
            writeln(e.Message);
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
   writeln('Exiting REPL');
end;

function TGoofyRepl.ReadUserInput: TStringList;
var
   buff: String;
begin
   write('> ');
   readln(buff);
   Result := TStringList.Create;
   Result.Add(buff);
end;

initialization
   
end.
