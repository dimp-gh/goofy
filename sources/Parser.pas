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
function ParseFile(path: String): TAST;
function ParseStringList(sl: TStringList): TAST;

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

function ParseFile(path: String): TAST;
var
   lines: TStringList;
begin
   // TODO: check is path exists
   lines := TStringList.Create;
   lines.LoadFromFile(path);
   Result := ParseStringList(lines);   
end;

function ParseStringList(sl: TStringList): TAST;
begin
   
end;

initialization
   
end.
