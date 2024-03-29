with Schedule;       use Schedule;
procedure Main is
   Classes : Classes_T;
begin
   -- Add some valid classes
   Classes.Add_Class (Name       => "Calculus  ",
                      Day        => Mon,
                      Start_Time => 10.0,
                      End_Time   => 11.0);
   Classes.Add_Class (Name       => "History   ",
                      Day        => Tue,
                      Start_Time => 11.0,
                      End_Time   => 12.5);
   Classes.Add_Class (Name       => "Biology   ",
                      Day        => Wed,
                      Start_Time => 13.0,
                      End_Time   => 14.0);
   Classes.Print;

   -- Add an invalid class and handle the exception
   Classes.Add_Class (Name       => "Chemistry ",
                      Day        => Thu,
                      Start_Time => 13.0,
                      End_Time   => 14.0);
end Main;
--Main
