with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

begin

   loop
      Put ("(A)dd | (R)emove | (P)rint | Q(uit) : ");
      declare
         Str : constant String := Get_Line;
      begin
         exit when Str'Length = 0;
         case Str (Str'First) is
            when 'A' =>
               -- Ask user for a legal value and add it to the list
               null;
            when 'R' =>
               -- Ask user for a legal value and remove it from the list
               null;
            when 'P' =>
               -- Print the list
               null;
            when 'Q' =>
               exit;
            when others =>
               Put_Line ("Illegal entry");
         end case;
      end;
   end loop;

end Main;
