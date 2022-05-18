-- Which statement(s) is(are) valid?

--$ begin question
with Pkg1; -- Defines tagged type Tag1, with primitive P
with Pkg2; use Pkg2; -- Defines tagged type Tag2, with primitive P
with Pkg3; -- Defines tagged type Tag3, with primitive P
use type Pkg3.Tag3;

procedure Main is
   O1 : Pkg1.Tag1;
   O2 : Pkg2.Tag2;
   O3 : Pkg3.Tag3;
--$ end question
begin
   --$ line cut
   O1.P;
   --$ line cut
   P (O1);
   --$ line cut
   P (O2);
   --$ begin cut
   P (O3);
   -- Only operators are :ada:`use`d, should have been :ada:`use all`
   --$ end cut
end Main;
