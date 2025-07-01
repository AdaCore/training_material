with Ada.Containers; use Ada.Containers;
with Loop_Types; use Loop_Types; use Loop_Types.Lists.Formal_Model;

package body Loop_Init is

   procedure Init_Table (T : out Table) is
   begin
      for J in T'Range loop
         T(J) := 0;
         pragma Loop_Invariant (for all K in T'First .. J => T(K)'Initialized);
         pragma Loop_Invariant (for all K in T'First .. J => T(K) = 0);
      end loop;
   end Init_Table;

   procedure Bump_Table (T : in out Table) is
   begin
      for J in T'Range loop
         T(J + 0) := T (J) + 1;
         pragma Loop_Invariant (for all K in T'First .. J => T(K) = T'Loop_Entry(K) + 1);
         pragma Loop_Invariant (for all K in J .. T'Last =>
                                  (if K > J then T(K) = T'Loop_Entry(K)));
      end loop;
   end Bump_Table;

   procedure Init_Vector (V : in out Vector) is
   begin
      for J in V.First_Index .. V.Last_Index loop
         V.Replace_Element (J, 0);
         pragma Loop_Invariant (V.Last_Index = V.Last_Index'Loop_Entry);
         pragma Loop_Invariant (for all K in V.First_Index .. J => V.Element (K) = 0);
      end loop;
   end Init_Vector;

   procedure Init_List (L : in out List) is
   begin
      for Cu in L loop
         L.Replace_Element (Cu, 0);
         pragma Loop_Invariant (for all I in 1 .. Positions (L).Get (Cu) =>
                                  Model (L).Get (I) = 0);
      end loop;
   end Init_List;

end Loop_Init;
