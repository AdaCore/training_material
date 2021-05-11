
with Ada.Text_IO; use Ada.Text_IO;
with Constants;
package body List is

   Content : array (1 .. Constants.Maximum_Count) of Integer;
   Last    : Natural := 0;

   procedure Add (Value : Integer) is
   begin
      if Last < Content'Last
      then
         Last           := Last + 1;
         Content (Last) := Value;
      else
         Put_Line ("Full");
      end if;
   end Add;

   procedure Remove (Value : Integer) is
   begin
      for I in 1 .. Last
      loop
         if Content (I) = Value
         then
            Content
              (I .. Last - 1) := Content
                (I + 1 .. Last);
            Last := Last - 1;
         end if;
      end loop;
   end Remove;

   procedure Print is
   begin
      for I in 1 .. Last
      loop
         Put_Line (Integer'Image (Content (I)));
      end loop;
   end Print;

   function Length return Natural is
   begin
      return Last;
   end Length;

end List;
