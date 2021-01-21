
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Schedule;       use Schedule;
procedure Main is
   Classes : Classes_T;
begin
   Classes.Add_Class
     (Name       => "Calculus",
      Day        => Mon,
      Start_Time => 10.0,
      End_Time   => 11.0);
   Classes.Add_Class
     (Name       => "History",
      Day        => Tue,
      Start_Time => 11.0,
      End_Time   => 12.5);
   Classes.Add_Class
     (Name       => "Biology",
      Day        => Wed,
      Start_Time => 13.0,
      End_Time   => 14.0);
   Classes.Print;
   begin
      Classes.Add_Class
        (Name       => "Biology",
         Day        => Thu,
         Start_Time => 13.0,
         End_Time   => 14.0);
   exception
      when The_Err : others =>
         Put_Line (Exception_Information (The_Err));
   end;
end Main;
