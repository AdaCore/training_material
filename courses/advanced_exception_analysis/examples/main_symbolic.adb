with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Exceptions;          use Ada.Exceptions;
with GNAT.Traceback;          use GNAT.Traceback;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Text_IO;             use Ada.Text_IO;
procedure Main_Symbolic is
   type Short_T is range -1_000 .. 1_000;
   Input : Short_T := Short_T'value (Ada.Command_Line.Argument (1));
   function One
     (Num : Short_T)
      return Short_T;

   function Three
     (Num : Short_T)
      return Short_T is (if Num < Short_T'last then One (Num + 300) else Num);
   function Two
     (Num : Short_T)
      return Short_T is
     (if Num < Short_T'last then Three (Num + 200) else Num);
   function One (Num : Short_T) return Short_T is
   begin
      Put_Line ("ONE: " &
                GNAT.Traceback.Symbolic.Symbolic_Traceback
                  (GNAT.Traceback.Call_Chain (10)));
      if Num < Short_T'last then
         return Two (Num + 100);
      else
         return Num;
      end if;
   end One;

begin
   Put_Line (Input'image & " => " & Short_T'image (Three (Input)));
exception
   when The_Err : others =>
      Put_Line ("FAILED: " & Exception_Name (The_Err) & " at: ");
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (The_Err));
end Main_Symbolic;
