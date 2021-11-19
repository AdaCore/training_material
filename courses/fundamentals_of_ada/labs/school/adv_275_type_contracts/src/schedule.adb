with Ada.Text_IO; use Ada.Text_IO;

package body Schedule is

   procedure Add_Class
     (Classes    : in out Classes_T;
      Name       :        String;
      Day        :        Days_T;
      Start_Time :        Time_T;
      End_Time   :        Time_T) is
   begin
      --  Implement the body
      null;
   end Add_Class;

   procedure Print (Classes : Classes_T) is
   begin
      for Index in 1 .. Classes.Size loop
         Put_Line
           (Days_T'Image (Classes.List (Index).Day) & ": " &
            To_String (Classes.List (Index).Name) & " (" &
            Time_T'Image (Classes.List (Index).Start_Time) & " -" &
            Time_T'Image (Classes.List (Index).End_Time) & " )");
      end loop;
   end Print;

end Schedule;
