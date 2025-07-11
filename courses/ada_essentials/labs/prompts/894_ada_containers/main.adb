with Ada.Text_IO; use Ada.Text_IO;
with City_Trivia;
procedure Main is

   function Get
     (Prompt : String)
      return String is
   begin
      Put (Prompt & "> ");
      return Get_Line;
   end Get;

begin

   Outer_Loop :
   loop
      declare
         City : constant String := Get ("City name");
      begin
         exit Outer_Loop when City'Length = 0;
         Inner_Loop :
         loop
            declare
               Info : constant String := Get ("  Trivia");
            begin
               exit Inner_Loop when Info'Length = 0;
               City_Trivia.Add_Trivia
                 (City        => City,
                  Information => Info);
            end;
         end loop Inner_Loop;
      end;
   end loop Outer_Loop;

   -- For each city entered, print the information entered
   --  for City of Cities loop
   --     Trivia := City_Trivia.Get_Trivia (City);
   --     Put_Line (City);
   --     for Info of Trivia loop
   --        Put_Line ("   " & Info);
   --     end loop;
   --  end loop;

end Main;
