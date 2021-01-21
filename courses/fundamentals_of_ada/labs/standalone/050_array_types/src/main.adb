with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type Days_Of_Week_T is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Unconstrained_Array_T is array (Days_Of_Week_T range <>) of Natural;

   Array1 : Unconstrained_Array_T := (1, 2, 3, 4, 5, 6, 7);
   Array2 : Unconstrained_Array_T (Days_Of_Week_T);

   type Name_T is array (1 .. 6) of Character;
   Weekly_Staff : array (Days_Of_Week_T, 1 .. 3) of Name_T;

begin

   Array2 := Array1;
   for Item of Array2
   loop
      Put_Line (Item'Image);
   end loop;
   New_Line;

   Array2 :=
     (Mon => 111, Tue => 222, Wed => 333, Thu => 444, Fri => 555, Sat => 666,
      Sun => 777);
   for Index in Array2'Range
   loop
      Put_Line (Index'Image & " => " & Array2 (Index)'Image);
   end loop;
   New_Line;

   Array2 (Mon .. Wed) := Array1 (Wed .. Fri);
   Array2 (Wed .. Fri) := (others => Natural'First);
   for Item of Array2
   loop
      Put_Line (Item'Image);
   end loop;
   New_Line;

   Weekly_Staff :=
     (Mon | Tue | Thu | Fri => ("Fred  ", "Barney", "Wilma "),
      Wed    => ("closed", "closed", "closed"),
      others => ("Pinky ", "Inky  ", "Blinky"));

   for Day in Weekly_Staff'Range (1)
   loop
      Put_Line (Day'Image);
      for Staff in Weekly_Staff'Range (2)
      loop
         Put_Line ("  " & String (Weekly_Staff (Day, Staff)));
      end loop;
   end loop;

end Main;
