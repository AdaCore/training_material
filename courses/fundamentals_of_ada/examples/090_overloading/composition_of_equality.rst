.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Composition_Of_Equality is
      type My_Integer is range -1_000 .. 1_000;
      function "=" (L, R : My_Integer) return Boolean is
        (False); -- for illustration purposes
      type Record_T is tagged record
         Component : My_Integer := 0;
      end record;
      type Record_Vector is array (My_Integer range 1 .. 10) of Record_T;
   
      I1, I2 : constant My_Integer  := 0;
      R1, R2 : constant Record_Vector := (others => (Component => 0));
   begin
      -- uses primitive "=" => False
      Put_Line (Boolean'Image (I1 = I2));
      -- uses predefined "=" for components=>True
      Put_Line (Boolean'Image (R1 = R2));
   end Composition_Of_Equality;
