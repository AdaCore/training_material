with Swaps; use Swaps;
with Ada.Text_IO;

package body Sorts is

   procedure Display_List (List : in Integer_List) is
      package IO renames Ada.Text_IO;
      procedure Put_String(S : String) renames Ada.Text_IO.Put;
   begin
      IO.Put ("(");
      for I in List'First .. List'Last loop
         if i /= List'First and then
           List (I) < 0 then
            Put_String (" ");
         end if;
         Ada.Text_IO.Put (Integer'Image (List (I)));
         if I /= List'Last then
            Ada.Text_IO.Put(",");
         end if;
      end loop;

      Ada.Text_IO.Put (")");
      Ada.Text_IO.New_Line;
   end Display_List;

   procedure Sort (List : in out Integer_List) is
      Idx_Min : Integer;
   begin

      for Current_Idx in List'First .. List'Last - 1 loop
         Idx_Min := Current_Idx;

         for Scan_Idx in Current_Idx + 1 .. List'Last loop
            if List (Scan_Idx) < List (Idx_Min) then
               Idx_Min := Scan_Idx;
            end if;
         end loop;

         if Current_Idx /= Idx_Min then
            Swap (List (Current_Idx), List (Idx_Min));
         end if;
      end loop;
   end Sort;

end Sorts;
