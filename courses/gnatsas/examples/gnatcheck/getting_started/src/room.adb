--::::::::::
--room.adb
--::::::::::
with Windows;
with Chop;
with Phil;
with Society;
with Calendar;
pragma Elaborate (Phil);
package body Room is
 
  -- Dining Philosophers, Ada 95 edition
  -- A line-oriented version of the Room package
  -- Michael B. Feldman, The George Washington University, 
  -- July, 1995.

  -- philosophers sign into dining room, giving Maitre_D their DNA code
 
  Dijkstra  : aliased Phil.Philosopher (My_ID => 1);
  Stroustrup: aliased Phil.Philosopher (My_ID => 2);
  Anderson  : aliased Phil.Philosopher (My_ID => 3);
  Ichbiah   : aliased Phil.Philosopher (My_ID => 4);
  Taft      : aliased Phil.Philosopher (My_ID => 5);
 
  type Philosopher_Ptr is access all Phil.Philosopher;

  Phils : array (Table_Type) of Philosopher_Ptr;
  Phil_Windows : array(Table_Type) of Windows.Window;
  Phil_Seats : array (Society.Unique_DNA_Codes) of Table_Type;

  task body Maitre_D is
 
    T          : Natural;
    Start_Time : Calendar.Time;
    Blanks : constant String := "     ";

  begin
 
    accept Start_Serving;

    Start_Time := Calendar.Clock;
 
    -- now Maitre_D assigns phils to seats at the table

    Phils :=
      (Dijkstra'Access,
       Anderson'Access,
       Taft'Access,
       Ichbiah'Access,
       Stroustrup'Access);
  
    Phil_Seats   := (1, 3, 5, 4, 2);   -- which seat each phil occupies

    Phil_Windows :=
      (Windows.Open (( 1, 24), 7, 30),
       Windows.Open (( 9,  2), 7, 30),
       Windows.Open (( 9, 46), 7, 30),
       Windows.Open ((17,  7), 7, 30),
       Windows.Open ((17, 41), 7, 30));


    for Which_Window in Phil_Windows'range loop
      Windows.Borders (Phil_Windows(Which_Window), '+', '|', '-');
    end loop;

    -- and assigns them their chopsticks.

    Phils (1).Start_Eating (1, 2);
    Phils (3).Start_Eating (3, 4);
    Phils (2).Start_Eating (2, 3);
    Phils (5).Start_Eating (1, 5);
    Phils (4).Start_Eating (4, 5);
 
    loop
      select
        accept Report_State (Which_Phil : in Society.Unique_DNA_Codes;
                             State      : in Phil.States;
                             How_Long   : in Natural := 0;
                             Which_Meal : in Natural := 0) do

          T := Natural (Calendar."-" (Calendar.Clock, Start_Time));
 
          case State is
 
            when Phil.Breathing =>
              Windows.Title(Phil_Windows(Phil_Seats(Which_Phil)),
                            Society.Name_Register(Which_Phil), '-');
              Windows.Put (Phil_Windows(Phil_Seats(Which_Phil)),
                     "T =" & Integer'Image (T) & " " 
                      & "Breathing...");
              Windows.New_Line (Phil_Windows(Phil_Seats(Which_Phil)));

            when Phil.Thinking =>
              Windows.Put (Phil_Windows(Phil_Seats(Which_Phil)),
                     "T =" & Integer'Image (T) & " " 
                      & "Thinking" 
                      & Integer'Image (How_Long) & " seconds.");
              Windows.New_Line (Phil_Windows(Phil_Seats(Which_Phil)));

            when Phil.Eating =>
              Windows.Put (Phil_Windows(Phil_Seats(Which_Phil)),
                     "T =" & Integer'Image (T) & " " 
                      & "Meal"  
                      & Integer'Image (Which_Meal)
                      & ","  
                      & Integer'Image (How_Long) & " seconds.");
              Windows.New_Line (Phil_Windows(Phil_Seats(Which_Phil)));

            when Phil.Done_Eating =>
              Windows.Put (Phil_Windows(Phil_Seats(Which_Phil)),
                     "T =" & Integer'Image (T) & " " 
                      & "Yum-yum (burp)");
              Windows.New_Line (Phil_Windows(Phil_Seats(Which_Phil)));

            when Phil.Got_One_Stick =>
              Windows.Put (Phil_Windows(Phil_Seats(Which_Phil)),
                     "T =" & Integer'Image (T) & " " 
                      & "First chopstick" 
                      & Integer'Image (How_Long));
              Windows.New_Line (Phil_Windows(Phil_Seats(Which_Phil)));

            when Phil.Got_Other_Stick =>
              Windows.Put (Phil_Windows(Phil_Seats(Which_Phil)),
                     "T =" & Integer'Image (T) & " " 
                      & "Second chopstick" 
                      & Integer'Image (How_Long));
              Windows.New_Line (Phil_Windows(Phil_Seats(Which_Phil)));

            when Phil.Dying =>
              Windows.Put (Phil_Windows(Phil_Seats(Which_Phil)),
                     "T =" & Integer'Image (T) & " " 
                      & "Croak");
              Windows.New_Line (Phil_Windows(Phil_Seats(Which_Phil)));

          end case; -- State
          
        end Report_State;
 
      or
        terminate;
      end select;
 
    end loop;
 
  end Maitre_D;
 
end Room;
