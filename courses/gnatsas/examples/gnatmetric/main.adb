with Coupling_Metrics_Dependency;
use Coupling_Metrics_Dependency;
with Coupling_Metrics_Example;
procedure Main is
   A : constant Record_T := Set (1, 2);
   B : constant Record_T := Set (30, 40);
begin
   Coupling_Metrics_Example.Example (A, B);
end Main;
