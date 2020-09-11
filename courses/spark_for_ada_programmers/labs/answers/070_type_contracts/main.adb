with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;
with Important_Dates; use Important_Dates;

procedure Main with
   SPARK_Mode => On
is

   Calendar : Calendar_T;

   procedure Run_Test is
   begin
      Put_Line ("Good result:");
      Print_Events
        (Calendar         => Calendar,
         Number_Of_Events => 10,
         Date             => (2_020, 4, 20));

      begin
         Put_Line ("Past the end:");
         Print_Events (Calendar, 5, (2_020, 12, 20));
      exception
         when The_Err : others =>
            Put_Line ("Exception " & Exception_Information (The_Err));
      end;

      begin
         Put_Line ("Predicate failure:");
         Print_Events (Calendar, 5, (2_020, 4, 31));
      exception
         when The_Err : others =>
            Put_Line ("Exception " & Exception_Information (The_Err));
      end;

   exception
      when The_Err : others =>
         Put_Line ("Unexpected Exception " & Exception_Information (The_Err));
   end Run_Test;

begin

   Add_Event (Calendar, "Administrative Professionals Day", (2_020, 4, 22));
   Add_Event (Calendar, "Air Force Birthday", (2_020, 9, 18));
   Add_Event (Calendar, "American Eagle Day", (2_020, 6, 20));
   Add_Event (Calendar, "Arbor Day", (2_020, 4, 24));
   Add_Event (Calendar, "Armed Forces Day", (2_020, 5, 16));
   Add_Event (Calendar, "Army Birthday", (2_020, 6, 14));
   Add_Event (Calendar, "Bastille Day", (2_020, 7, 14));
   Add_Event (Calendar, "Belmont Stakes", (2_020, 6, 6));
   Add_Event (Calendar, "Bennington Battle Day", (2_020, 8, 16));
   Add_Event (Calendar, "Bill of Rights Day", (2_020, 12, 15));
   Add_Event (Calendar, "Black Friday", (2_020, 11, 27));
   Add_Event (Calendar, "Boss's Day", (2_020, 10, 16));
   Add_Event (Calendar, "Child Health Day", (2_020, 10, 5));
   Add_Event (Calendar, "Chinese New Year", (2_020, 1, 25));
   Add_Event (Calendar, "Christmas Day", (2_020, 12, 25));
   Add_Event (Calendar, "Coast Guard Birthday", (2_020, 8, 4));
   Add_Event (Calendar, "Colorado Day", (2_020, 8, 1));
   Add_Event (Calendar, "Columbus Day", (2_020, 10, 12));
   Add_Event
     (Calendar, "Constitution Day and Citizenship Day", (2_020, 9, 17));
   Add_Event (Calendar, "Cyber Monday", (2_020, 11, 30));
   Add_Event (Calendar, "Daylight Saving Time ends", (2_020, 11, 1));
   Add_Event (Calendar, "Daylight Saving Time starts", (2_020, 3, 8));
   Add_Event (Calendar, "December Solstice", (2_020, 12, 21));
   Add_Event (Calendar, "Election Day", (2_020, 11, 3));
   Add_Event (Calendar, "Elizabeth Peratrovich Day", (2_020, 2, 16));
   Add_Event
     (Calendar, "Emergency Medical Services for Children Day", (2_020, 5, 20));
   Add_Event (Calendar, "Employee Appreciation Day", (2_020, 3, 6));
   Add_Event (Calendar, "Evacuation Day", (2_020, 3, 17));
   Add_Event (Calendar, "Family Day", (2_020, 11, 27));
   Add_Event (Calendar, "Father's Day", (2_020, 6, 21));
   Add_Event (Calendar, "Flag Day", (2_020, 6, 14));
   Add_Event (Calendar, "Gold Star Mother's Day", (2_020, 9, 27));
   Add_Event (Calendar, "Groundhog Day", (2_020, 2, 2));
   Add_Event (Calendar, "Halloween", (2_020, 10, 31));
   Add_Event (Calendar, "Independence Day", (2_020, 7, 4));
   Add_Event (Calendar, "June Solstice", (2_020, 6, 20));
   Add_Event (Calendar, "Kentucky Derby", (2_020, 5, 2));
   Add_Event (Calendar, "Labor Day", (2_020, 9, 7));
   Add_Event (Calendar, "Law Day", (2_020, 5, 1));
   Add_Event (Calendar, "Leif Erikson Day", (2_020, 10, 9));
   Remove_Event (Calendar, "Child Health Day", (2_020, 10, 5));
   Add_Event (Calendar, "Lincoln's Birthday", (2_020, 2, 12));
   Add_Event (Calendar, "Linus Pauling Day", (2_020, 2, 28));
   Remove_Event (Calendar, "My Birthday", (2_020, 1, 1));
   Add_Event (Calendar, "March Equinox", (2_020, 3, 19));
   Add_Event (Calendar, "Marine Corps Birthday", (2_020, 11, 10));
   Add_Event (Calendar, "Martin Luther King Jr. Day", (2_020, 1, 20));
   Remove_Event (Calendar, "Colorado Day", (2_020, 8, 1));
   Add_Event (Calendar, "Memorial Day", (2_020, 5, 25));
   Add_Event (Calendar, "Military Spouse Appreciation Day", (2_020, 5, 8));
   Remove_Event (Calendar, "Daylight Saving Time ends", (2_020, 11, 1));
   Add_Event (Calendar, "Mother's Day", (2_020, 5, 10));
   Add_Event (Calendar, "National Aviation Day", (2_020, 8, 19));
   Add_Event (Calendar, "National CleanUp Day", (2_020, 9, 19));
   Remove_Event (Calendar, "Groundhog Day", (2_020, 2, 2));
   Add_Event (Calendar, "Navy Birthday", (2_020, 10, 13));
   Add_Event (Calendar, "New Year's Day", (2_020, 1, 1));
   Add_Event (Calendar, "New Year's Eve", (2_020, 12, 31));
   Add_Event (Calendar, "New York City Marathon", (2_020, 11, 1));
   Remove_Event (Calendar, "Independence Day", (2_020, 7, 4));
   Remove_Event (Calendar, "Labor Day", (2_020, 9, 7));
   Add_Event (Calendar, "Pan American Aviation Day", (2_020, 12, 17));
   Add_Event (Calendar, "Parents' Day", (2_020, 7, 26));
   Add_Event (Calendar, "Patriot's Day", (2_020, 4, 20));
   Add_Event (Calendar, "Peace Officers Memorial Day", (2_020, 5, 15));
   Add_Event (Calendar, "Pioneer Day", (2_020, 7, 24));
   Add_Event (Calendar, "Preakness Stakes", (2_020, 5, 16));
   Remove_Event (Calendar, "Law Day", (2_020, 5, 1));
   Add_Event (Calendar, "Presidents' Day", (2_020, 2, 17));
   Add_Event (Calendar, "Purple Heart Day", (2_020, 8, 7));
   Add_Event (Calendar, "Read Across America Day", (2_020, 3, 2));
   Add_Event (Calendar, "Senior Citizens Day", (2_020, 8, 21));
   Add_Event (Calendar, "September Equinox", (2_020, 9, 22));
   Add_Event (Calendar, "Stephen Foster Memorial Day", (2_020, 1, 13));
   Remove_Event (Calendar, "New Year's Day", (2_020, 1, 1));
   Add_Event (Calendar, "Super Bowl", (2_020, 2, 2));
   Add_Event (Calendar, "Super Tuesday", (2_020, 3, 3));
   Remove_Event (Calendar, "Read Across America Day", (2_020, 3, 2));
   Add_Event (Calendar, "Susan B. Anthony's Birthday", (2_020, 2, 15));
   Add_Event (Calendar, "Thanksgiving Day", (2_020, 11, 26));
   Add_Event (Calendar, "Thomas Jefferson's Birthday", (2_020, 4, 13));
   Remove_Event (Calendar, "Thomas Jefferson's Birthday", (2_020, 4, 13));
   Add_Event (Calendar, "Truman Day", (2_020, 5, 8));
   Add_Event (Calendar, "Valentine's Day", (2_020, 2, 14));
   Remove_Event (Calendar, "Bill of Rights Day", (2_020, 12, 15));
   Add_Event (Calendar, "Veterans Day", (2_020, 11, 11));
   Add_Event (Calendar, "Wright Brothers Day", (2_020, 12, 17));
   Remove_Event (Calendar, "Belmont Stakes", (2_020, 6, 6));

   Run_Test;

end Main;
