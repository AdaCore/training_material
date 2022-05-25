with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
procedure Main is
   type Short_T is range -1_000 .. 1_000;
   Input : Short_T := Short_T'value (Ada.Command_Line.Argument (1));
   function One (Num : Short_T) return Short_T;

   function Three (Num : Short_T) return Short_T is
     (if Num < Short_T'last then One (Num + 300) else Num);
   function Two (Num : Short_T) return Short_T is
     (if Num < Short_T'last then Three (Num + 200) else Num);
   function One (Num : Short_T) return Short_T is
     (if Num < Short_T'last then Two (Num + 100) else Num);

begin
   Put_Line (Input'image & " => " & Short_T'image (Three (Input)));
end Main;
