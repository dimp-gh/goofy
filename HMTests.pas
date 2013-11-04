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
   tpe := TOper.Create('OneOfThree', [ts.GenerateVariable, ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = 'OneOfThree d e f');
end;

procedure IsIntegerLiteralTest;
begin
   Assert(IsIntegerLiteral('123'));
   Assert(IsIntegerLiteral('256'));
   Assert(Not IsIntegerLiteral('abc'));
end;

initialization
   TypePrintingTest;
   IsIntegerLiteralTest;
end.
