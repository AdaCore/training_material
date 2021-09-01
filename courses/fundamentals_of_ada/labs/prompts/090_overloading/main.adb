with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   --Conversion_Functions
   type Digit_T is range 0 .. 9;
   type Digit_Name_T is
     (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine);

   -- functions to convert between Digit_T, Digit_Name_T, and input values
   -- function Convert ( ?

   -- functions to compare across types
   --  function "=" ( ?

   Last_Entry : Digit_T := 0;

begin
   loop
      Put ("Input: ");
      declare
         Str : constant String := Get_Line;
      begin
         exit when Str'Length = 0;
         -- If the string is a number
         --    Convert the entry to a name and print it
         --    if this input has the same value as the previous value, say so
         -- Else (this is a name)
         --    Convert the entry to a number and print it
         --    if this input has the same value as the previous value, say so
      end;
   end loop;
end Main;
--Main
