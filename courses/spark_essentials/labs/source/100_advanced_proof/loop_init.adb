package body Loop_Init is

   procedure Init_Table (T : out Table) is
   begin
      for J in T'Range loop
         T(J) := 0;
      end loop;
   end Init_Table;

   procedure Bump_Table (T : in out Table) is
   begin
      for J in T'Range loop
         T(J) := T(J) + 1;
      end loop;
   end Bump_Table;

   procedure Init_Vector (V : in out Vector) is
   begin
      for J in V.First_Index .. V.Last_Index loop
         V.Replace_Element (J, 0);
      end loop;
   end Init_Vector;

   procedure Init_List (L : in out List) is
   begin
      for Cu in L loop
         L.Replace_Element (Cu, 0);
      end loop;
   end Init_List;
end Loop_Init;
