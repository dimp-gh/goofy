unit HMTests;
{$ASSERTIONS ON}
interface

uses HMTypes, HindleyMilner;

implementation

procedure TypePrintingTest;
var
   gen: TGenerator;
   tpe: TType;
   ts: TTypeSystem;
begin
   gen := TGenerator.Create;
   ts := TTypeSystem.Create(Pointer(gen));
   tpe := TOper.Create('Maybe', [ts.GenerateVariable]);
   Assert(tpe.ToStr = 'Maybe a');
   tpe := TOper.Create('->', [ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = '(b -> c)');
   tpe := TOper.Create('OneOfThree', [ts.GenerateVariable, ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = 'OneOfThree d e f');
end;

initialization
   TypePrintingTest;
end.
