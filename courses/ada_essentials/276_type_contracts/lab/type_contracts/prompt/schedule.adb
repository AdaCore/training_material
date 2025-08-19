package body Schedule is

   procedure Add_Class
     (Classes    : in out Classes_T;
      Name       :        Name_T;
      Day        :        Days_T;
      Start_Time :        Time_T;
      End_Time   :        Time_T) is null;

   procedure Print (Classes : Classes_T) is null;

   function Count (Classes : Classes_T) return Natural is (0);
end Schedule;
