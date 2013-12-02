program Tests;
uses
   AST in '../sources/AST.pas',
   HMDataStructures in '../sources/HMDataStructures.pas',
   HMTypes in '../sources/HMTypes.pas',
   HindleyMilner in '../sources/HindleyMilner.pas',
   Values in '../sources/Values.pas',
   ValueEnvironment in '../sources/ValueEnvironment.pas',
   Builtins in '../sources/Builtins.pas',
   GoofyTypeSystem in '../sources/GoofyTypeSystem.pas',
   ASTTests,
   HMTypesTests,
   HMDataStructuresTests,
   GoofyTypeSystemTests;

begin
   writeln('All tests passed');
end.

