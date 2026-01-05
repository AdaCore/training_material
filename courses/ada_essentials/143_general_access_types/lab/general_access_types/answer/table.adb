with Ada.Text_IO; use Ada.Text_IO;
with Database;

package body Table is

   use type Database.Value_T;

   type Sorted_T is record
      Value : Database.Value_T;
      Count : Database.Reference_T;
   end record;

   Sorted : array (Database.Value_T'Range) of Sorted_T;

   procedure Initialize is
   begin
      for Index in Sorted'Range loop
         Sorted (Index) := (Index, Database.Reference (Index));
      end loop;
   end Initialize;

   procedure Sort is
      Temp : Sorted_T;
   begin
      for J in Sorted'First .. Sorted'Last loop
         for K in J + 1 .. Sorted'Last loop
            if Sorted (K).Count.all < Sorted (K - 1).Count.all then
               Temp := Sorted (K);
               Sorted (K) := Sorted (K - 1);
               Sorted (K - 1) := Temp;
            end if;
         end loop;
      end loop;
   end Sort;

   procedure Print (Message : String) is
   begin
      Sort;
      Put_Line (Message);

      Put ("  Value: ");
      for Index in Sorted'Range loop
         Set_Col (8 + Positive_Count (Index * 4));
         Put (Sorted (Index).Value'Image);
      end loop;
      New_Line;

      Put ("  Count: ");
      for Index in Sorted'Range loop
         Set_Col (8 + Positive_Count (Index * 4));
         Put (Sorted (Index).Count.all'Image);
      end loop;
      New_Line;
   end Print;

end Table;
