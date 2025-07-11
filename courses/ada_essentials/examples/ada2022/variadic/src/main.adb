with Interfaces.C; use Interfaces.C;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Main is
   NUL renames Ada.Characters.Latin_1.NUL;
   --$ begin cut
   procedure printf (format : String; opt_param : int)
      with Import, Convention => C_Variadic_1; -- Note the 1 for a single arg
   --$ end cut
begin

   printf ("Value is %d" & LF & NUL, 20);
   
end Main;
