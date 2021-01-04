-----------
Array Lab
-----------

* Requirements

   - Create an array type whose index is days of the week and each element is a number
   - Create two objects of the array type, one of which is constant
   - Perform the following operations

      + Copy the constant object to the non-constant object and 
      + Print the contents of the non-constant object
      + Use an array aggregate to initialize the non-constant object
      + For each element of the array, print the array index and the value
      + Move part ("source") of the non-constant object to another part ("destination"), and then clear the source location
      + Print the contents of the non-constant object

* Hints

   - When you want to combine multiple strings (which are arrays!) use the concatenation operator (`&`)
   - Slices are how you access part of an array
   - Use aggregates (either named or positional) to initialize data

---------------------
Multiple Dimensions
---------------------

* Requirements

   - For each day of the week, you need an array of three strings containing names of workers for that day
   - Two sets of workers: weekend and weekday, but the store is closed on Wednesday (no workers)
   - Initialize the array and then print it hierarchically

-----------------------------------
Array Lab Solution (Declarations)
-----------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Main is
   
      type Days_Of_Week_T is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      type Unconstrained_Array_T is array
        (Days_Of_Week_T range <>) of Natural;
   
      Array1 : Unconstrained_Array_T := (1, 2, 3, 4, 5, 6, 7);
      Array2 : Unconstrained_Array_T (Days_Of_Week_T);
   
      type Name_T is array (1 .. 6) of Character;
      Weekly_Staff : array (Days_Of_Week_T, 1 .. 3) of Name_T;
   
   begin
   
-------------------------------------
Array Lab Solution (Implementation)
-------------------------------------

.. code:: Ada

      Array2 := Array1;
      for Item of Array2 loop
         Put_Line (Item'Image);
      end loop;
      New_Line;
   
      Array2 := (Mon => 111, Tue => 222, Wed => 333, Thu => 444, Fri => 555, Sat => 666, Sun => 777);
      for Index in Array2'Range loop
         Put_Line (Index'Image & " => " & Array2 (Index)'Image);
      end loop;
      New_Line;
   
      Array2 (Mon .. Wed) := Array1 (Wed .. Fri);
      Array2 (Wed .. Fri) := (others => Natural'First);
      for Item of Array2 loop
         Put_Line (Item'Image);
      end loop;
      New_Line;
   
      Weekly_Staff :=
        (Mon | Tue | Thu | Fri => ("Fred  ", "Barney", "Wilma "),
         Wed    => ("closed", "closed", "closed"),
         others => ("Pinky ", "Inky  ", "Blinky"));
   
      for Day in Weekly_Staff'Range (1) loop
         Put_Line (Day'Image);
         for Staff in Weekly_Staff'Range (2) loop
            Put_Line ("  " & String (Weekly_Staff (Day, Staff)));
         end loop;
      end loop;
