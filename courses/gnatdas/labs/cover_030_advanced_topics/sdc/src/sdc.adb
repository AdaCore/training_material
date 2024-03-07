with Ada.Exceptions;
with Except;
with Screen_Output;    use Screen_Output;
with Stack;
with Tokens;           use Tokens;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure SDC is
   File : File_Type;
begin
   Msg ("Welcome to SDC. Go ahead type your commands ...");

   if Argument_Count = 1 then
      begin
         Open (File, In_File, Argument (1));
      exception
         when Use_Error | Name_Error =>
            Error_Msg ("Could not open input file, exiting.");
            return;
      end;

      Set_Input (File);
   end if;

   loop
      --  Open a block to catch Stack Overflow and Underflow exceptions.

      begin

         Process (Next);
         --  Read the next Token from the input and process it.

      exception
         when Stack.Underflow =>
            Error_Msg ("Not enough values in the Stack.");

         when Stack.Overflow =>
            null;
      end;

   end loop;

exception
   when Except.Exit_SDC =>
      Msg ("Thank you for using SDC.");

   when The_Err : others =>
      Ada.Text_IO.Put_Line
        (">> " & Ada.Exceptions.Exception_Information (The_Err));
      Msg ("*** Internal Error ***.");

end SDC;
