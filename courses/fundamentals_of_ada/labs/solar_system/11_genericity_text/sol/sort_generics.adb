with Ada.Text_IO;
with Swap_Generics;

package body Sort_Generics is

   procedure Sort_Generic (List : in out List_Type) is

      Idx_Min : Integer;
      procedure Swap is new Swap_Generics.Swap_Generic (Data_Type => Element_Type);
   begin

      for Current_Idx in List'First .. List'Last - 1 loop
         Idx_Min := Current_Idx;

         for Scan_Idx in Current_Idx + 1 .. List'Last loop
            if Compare (List (Scan_Idx), List (Idx_Min)) then
               Idx_Min := Scan_Idx;
            end if;
         end loop;

         if Current_Idx /= Idx_Min then
            Swap (List (Current_Idx), List (Idx_Min));
         end if;
      end loop;
   end Sort_Generic;

   procedure Display_List (List : in List_Type) is
      package IO renames Ada.Text_IO;
      procedure Put_String(S : String) renames Ada.Text_IO.Put;
   begin
      IO.Put ("(");
      for I in List'First .. List'Last loop
         Ada.Text_IO.Put (To_String(List (I)));
         if I /= List'Last then
            Ada.Text_IO.Put(",");
         end if;
      end loop;

      Ada.Text_IO.Put (")");
      Ada.Text_IO.New_Line;
   end Display_List;

end Sort_Generics;
