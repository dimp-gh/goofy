unit Parser;
{$mode objfpc}{$H+}
interface

uses
   AST,
   SysUtils,
   TypInfo,
   Classes,
   expr in 'sources/parser/expr.pas';

function ParseString(s: String): TAST;
function ParseModule(path: String): TModule;

implementation

function ParseString(s: String): TAST;
var
   parser: TExprParser;
   StrStream: TStringStream;
begin
   parser := TExprParser.Create;
   StrStream := TStringStream.Create('');
   StrStream.WriteString(s);
   StrStream.Position := 0;
   parser.Parse(StrStream);
   Result := parser.parsed;
end;

function ParseModule(path: String): TModule;
var
   stream: TFileStream;
   parser: TExprParser;
   ast: TAST;
begin
   parser := TExprParser.Create;
   // TODO: check is path exists
   stream := TFileStream.Create(path, fmOpenRead);
   stream.Position := 0;
   try
      parser.Parse(stream);
   except
      on e: EExprParserException do
      begin
         stream.Destroy;
         raise EExprParserException.Create(e.Message);
      end;
   end;
   stream.Destroy;
   ast := parser.parsed;
   if (ast is TExpression) or (ast is TStatement) then
      raise EExprParserException.Create('Module content is neither expression nor statement');
   Result := ast as TModule;
end;

initialization
   
end.
