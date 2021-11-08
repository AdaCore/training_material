with Ada.Text_IO; use Ada.Text_IO;

package body Protected_Objects is

   protected body Object is

      procedure Initialize (My_Id : Character) is
      begin
         Id := My_Id;
      end Initialize;
      
      procedure Set (Caller : Character; V : Integer) is
      begin
         Local := V;
         Put_Line ( "Task-" & Caller & " Object-" & Id & " => " & V'Image );
      end Set;

      function Get return Integer is
      begin
         return Local;
      end Get;
   end Object;

end Protected_Objects;
