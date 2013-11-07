unit HMTests;
{$ASSERTIONS ON}
interface

uses HMTypes, HindleyMilner;

implementation

procedure TypePrintingTest;
var
   tpe: TType;
   ts: THMTypeSystem;
begin
   ts := THMTypeSystem.Create;
   tpe := TParameterizedType.Create('Maybe', [ts.GenerateVariable]);
   Assert(tpe.ToStr = 'Maybe a');
   ts.ResetNameGenerator;
   tpe := TParameterizedType.Create('->',
       [ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = '(a -> b)');
   ts.ResetNameGenerator;
   tpe := TParameterizedType.Create('Either',
       [ts.GenerateVariable, ts.GenerateVariable]);
   Assert(tpe.ToStr = 'Either a b');
   tpe := TParameterizedType.Create('OneOfThree',
       [ts.GenerateVariable, ts.GenerateVariable, ts.GenerateVariable]);
   ts.ResetNameGenerator;
   Assert(tpe.ToStr = 'OneOfThree a b c');
end;

initialization
   TypePrintingTest;
end.
