with Ada.Text_IO; use Ada.Text_IO;
with Timecards;
procedure Main is

   one : Timecards.Timecard_T := timecards.Create (Name  => "Fred Flintstone",
                                                   Rate  => 1.1,
                                                   Hours => 2.2);
        two : Timecards.Timecard_T := timecards.Create (Name  => "Barney Rubble",
                                                   Rate  => 3.3,
                                                   Hours => 4.4);


begin

   put_line (timecards.image ( one ) );
   put_line ( timecards.image ( two ) );

end Main;
