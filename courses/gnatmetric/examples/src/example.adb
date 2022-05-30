with Ada.Text_IO; use Ada.Text_IO;
package body Example is

   procedure Internal (C : Character) is
   begin
      Put (C);
   end Internal;

   -- Print the prompt
   procedure Example (S1, S2 : String_T) is
      S : constant String := To_String (S1 & S2);
   begin
      for C of S
      loop
         Internal (C);
      end loop;
      New_Line;
   end Example;

end Example;
