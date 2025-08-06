.. code:: Ada

   declare
       type T2 is new T with null record;
       A : T'Class := T2'(null record);
   begin
       Prim (A);
       Not_Prim (A);
   end;
