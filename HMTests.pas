unit HMTests;
{$ASSERTIONS ON}
interface

uses HMTypes, HindleyMilner;

implementation

procedure TypePrintingTest;
var
   tpe: TType;
   ts: TTypeSystem;
begin
   ts := TTypeSystem.Create;
   tpe := TOper.Create('Maybe', [ts.GenerateVariable]);
   Assert(tpe.ToStr = 'Maybe a');
   tpe := TOper.Create('->', [ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = '(b -> c)');
   tpe := TOper.Create('Either', [ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = 'Either d e');
   tpe := TOper.Create('OneOfThree', [ts.GenerateVariable, ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = 'OneOfThree f g h');
end;

procedure IsIntegerLiteralTest;
begin
   Assert(IsIntegerLiteral('123'));
   Assert(IsIntegerLiteral('256'));
   Assert(Not IsIntegerLiteral('abc'));
   Assert(Not IsIntegerLiteral('0xB00B1E5'));
end;

initialization
   TypePrintingTest;
   IsIntegerLiteralTest;
end.
