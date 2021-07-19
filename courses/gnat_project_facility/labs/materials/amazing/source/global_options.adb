-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with GNAT.IO;           use GNAT.IO;
with Ada.Command_Line;  use Ada.Command_Line;

package body Global_Options is

   Arguments_Error : exception;
   --  Raised by Parse_Command_Line.

   procedure Parse_Command_Line;
   --  Parses the command line switches, setting the globals vars as specified.
   --  Raises Arguments_Error.
   --  Checks that the required switches are present and that conflicting
   --  switches are not specified.

   function Invalid_Arg (Arg_Num : Natural) return Boolean;
   --  Verifies that Arg_Num is not greater than the total number of arguments
   --  and that the arg at this number is not a switch (ie does not start with
   --  "-")

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      Current_Arg : Positive := 1;
   begin
      while Current_Arg <= Argument_Count loop

         --  the width of a newly generated maze
         if Argument (Current_Arg) = "-w" then
            if not Filename_Set then
               Current_Arg := Current_Arg + 1;
               if Invalid_Arg (Current_Arg) then
                  Put_Line ("No maze width specified along with -w switch");
                  raise Arguments_Error;
               else
                  begin
                     Width := Integer'Value (Argument (Current_Arg));
                  exception
                     when others =>
                        Put_Line ("Invalid integer argument for -w switch");
                        raise Arguments_Error;
                  end;
               end if;
               Width_Set := True;
            else --  they already specified a file name so this conflicts
               Put_Line ("""-w"" found but a file name was already specified");
               raise Arguments_Error;
            end if;

            --  the height of a newly generated maze
         elsif Argument (Current_Arg) = "-h" then
            if not Filename_Set then
               Current_Arg := Current_Arg + 1;
               if Invalid_Arg (Current_Arg) then
                  Put_Line ("No maze height specified along with -h switch");
                  raise Arguments_Error;
               else
                  begin
                     Height := Integer'Value (Argument (Current_Arg));
                  exception
                     when others =>
                        Put_Line ("Invalid integer argument for -h switch");
                        raise Arguments_Error;
                  end;
               end if;
               Height_Set := True;
            else --  they already specified a file name so this conflicts
               Put_Line ("""-h"" found but a file name was already specified");
               raise Arguments_Error;
            end if;

            --  the file name of an existing maze
         elsif Argument (Current_Arg) = "-f" then
            if Width_Set or Height_Set then
               Put_Line ("A width and/or height was already specified");
               raise Arguments_Error;
            elsif not Filename_Set then
               Current_Arg := Current_Arg + 1;
               if Invalid_Arg (Current_Arg) then
                  Put_Line ("No file name specified along with -f switch");
                  raise Arguments_Error;
               else
                  File_Name := To_Unbounded_String (Argument (Current_Arg));
               end if;
               Filename_Set := True;
            else --  they already specified a file name so this conflicts
               Put_Line ("""-f"" found but a file name was already specified");
               raise Arguments_Error;
            end if;

            --  the number of searcher threads to use
         elsif Argument (Current_Arg) = "-t" then
            Current_Arg := Current_Arg + 1;
            if Invalid_Arg (Current_Arg) then
               Put_Line ("No thread count specified along with -t switch");
               raise Arguments_Error;
            else
               begin
                  Max_Searchers := Integer'Value (Argument (Current_Arg));
               exception
                  when others =>
                     Put_Line ("Invalid integer argument for -t switch");
                     raise Arguments_Error;
               end;
            end if;

            --  whether the newly generated maze is to be "perfect" (only 1
            --  solution)
         elsif Argument (Current_Arg) = "-p" then
            Perfect := True;

            --  whether to display output (-q means "quiet")
         elsif Argument (Current_Arg) = "-q" then
            Display_Output := False;

         else
            Put_Line ("Unknown switch " & Argument (Current_Arg));
            raise Arguments_Error;
         end if;

         Current_Arg := Current_Arg + 1;
      end loop;

      if not (Filename_Set or (Width_Set and Height_Set)) then
         Put_Line ("Incomplete required switch settings.");
         raise Arguments_Error;
      end if;

      if not Filename_Set and not Display_Output then
         Put_Line ("It is nonsensical to create a new maze but not show it!");
         raise Arguments_Error;
      end if;
   end Parse_Command_Line;

   -----------------
   -- Invalid_Arg --
   -----------------

   function Invalid_Arg (Arg_Num : Natural) return Boolean is
   begin
      return Arg_Num > Argument_Count or else Argument (Arg_Num) (1) = '-';
   end Invalid_Arg;

begin
   Parse_Command_Line;
exception
   when Arguments_Error =>
      Arguments_Invalid := True;
end Global_Options;
