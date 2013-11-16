unit HMTypesTests;
{$ASSERTIONS ON}
interface

uses HMTypes;

implementation

procedure NameGeneratorTest;
var gen: TNameGenerator;
begin
   gen := TNameGenerator.Create('a');
   Assert(gen.GenerateName = 'a');
   Assert(gen.GenerateName = 'b');
   Assert(gen.GenerateName = 'c');
end;

procedure TypePrintingTest;
var
   tpe: TType;
   vg: TVariableGenerator;
begin
   vg := TVariableGenerator.Create;
   tpe := TParameterizedType.Create('Maybe',
                                    [vg.GenerateVariable]);
   Assert(tpe.ToStr = 'Maybe a');
   vg.ResetNameGenerator;
   tpe := TParameterizedType.Create('->',
                                    [vg.GenerateVariable,
                                     vg.GenerateVariable]);
   Assert(tpe.ToStr = '(a -> b)');
   vg.ResetNameGenerator;
   tpe := TParameterizedType.Create('Either',
                                    [vg.GenerateVariable,
                                     vg.GenerateVariable]);
   Assert(tpe.ToStr = 'Either a b');
   tpe := TParameterizedType.Create('OneOfThree',
                                    [vg.GenerateVariable,
                                     vg.GenerateVariable,
                                     vg.GenerateVariable]);
   vg.ResetNameGenerator;
   Assert(tpe.ToStr = 'OneOfThree a b c');
end;

initialization
   NameGeneratorTest;
   TypePrintingTest;
end.
