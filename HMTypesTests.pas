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

initialization
   NameGeneratorTest;
end.
