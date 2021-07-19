--
--            Copyright (C) 2008-2010, AdaCore
--

with Maze.IO;
with GNAT.IO;         use GNAT.IO;
with Global_Options;  use Global_Options;
package body HCI is

   ----------------
   -- Show_Usage --
   ----------------

   procedure Show_Usage is
   begin
      New_Line;
      Put_Line ("Usage:");
      New_Line;
      Put_Line ("  <mice/mouse> <arguments>");
      New_Line;
      Put_Line ("Required arguments:");
      New_Line;
      Put_Line ("  -w <positive>  width of a new maze to be generated");
      Put_Line ("  -h <positive>  height of a new maze to be generated");
      New_Line;
      Put_Line ("  or, alternatively:");
      New_Line;
      Put_Line ("  -f <file-name>  a file containing an existing maze");
      New_Line;
      Put_Line ("Optional arguments:");
      New_Line;
      Put_Line ("  -t <positive>  the number of searcher threads;" &
                " the default is" & Max_Searchers'Img);
      New_Line;
      Put_Line ("  -p             the new maze is to be ""perfect"" " &
                "(have only 1 exit)");
      New_Line;
      Put_Line ("  -q             be quiet (do not display output)");
      New_Line;
      Put_Line ("Notes:");
      New_Line;
      Put_Line ("  ""mice"" is the multi-threaded maze solver.");
      New_Line;
      Put_Line ("  ""mouse"" is single-threaded so ""-t"" has no effect");
      New_Line;
   end Show_Usage;

   --------------------------
   -- Optionally_Save_Maze --
   --------------------------

   procedure Optionally_Save_Maze  (Unsaved : Boolean) is
      Answer : String (1 .. 80);
      Last   : Integer;
   begin
      if Unsaved then
         New_Line (2);
         Put ("Do you want to save this maze? ");
         Get_Line (Answer, Last);
         if Last /= 0 then
            if Answer (1) = 'y' or Answer (1) = 'Y' then
               loop
                  Put ("Under what name? ");
                  Get_Line (Answer, Last);
                  exit when Last /= 0;
               end loop;
               Maze.IO.Save_Maze (Answer (1 .. Last));
               Put_Line ("Done.");
            end if;
         end if;
      end if;
   end Optionally_Save_Maze;

end HCI;
