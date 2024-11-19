with Ada.Text_IO; use Ada.Text_IO;
package body Line_Metrics_Example is

   procedure Internal (C : Character) is
   begin
      Put (C);
   end Internal;

   -- Print the prompt
   procedure Example (S1, S2 : String) is
      S : constant String := S1 & S2;
   begin
      for C of S loop
         Internal (C);
      end loop;
      New_Line;
   end Example;

end Line_Metrics_Example;
