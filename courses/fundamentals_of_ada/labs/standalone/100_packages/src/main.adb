
with Ada.Text_IO; use Ada.Text_IO;
with Input;
with List;
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
               List.Add (Input.Get_Value ("Value to add"));
            when 'R' =>
               List.Remove (Input.Get_Value ("Value to remove"));
            when 'P' =>
               List.Print;
            when 'Q' =>
               exit;
            when others =>
               Put_Line ("Illegal entry");
         end case;
      end;
   end loop;

end Main;
