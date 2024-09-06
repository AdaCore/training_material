-- Which proposition(s) is (are) legal and running without error?

procedure Main is
   --$ begin question
   type T1 is new Integer;
   function "+" (A : T1) return T1 is (0);
   type T2 is new T1;
   type T3 is new T1;
   overriding function "+" (A : T3) return T3 is (1);

   O1 : T1;
   O2 : T2;
   O3 : T3;
   --$ end question

begin
   --$ line cut
   pragma Assert (+O1 = 0);
   --$ line cut
   pragma Assert (+O2 = 0);
   --$ begin cut
   pragma Assert ((+O2) + (+O3) = 1);
   -- ``+O2`` returns a ``T2``, ``+O3`` a ``T3``
   --$ end cut
   --$ line cut
   pragma Assert (+(T3 (O1) + O3) = 1);
end Main;
