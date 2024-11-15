with Coupling_Metrics_Dependency; use Coupling_Metrics_Dependency;

with Coupling_Metrics_Example;
with Complexity_Metrics_Example;
with Line_Metrics_Example;
with Syntax_Metrics_Example;

procedure Main is
   Coupling_Object_A : constant Record_T := Set (1, 2);
   Coupling_Object_B : constant Record_T := Set (30, 40);
   Complexity_Object : String            := "Hello World";
   Syntax_Object     : Syntax_Metrics_Example.String_T;
begin
   Coupling_Metrics_Example.Example (Coupling_Object_A, Coupling_Object_B);
   Complexity_Metrics_Example.Example (Complexity_Object);
   Line_Metrics_Example.Example ("Hello", "World");
   Syntax_Object := Syntax_Metrics_Example.From_String ("Hello");
end Main;
