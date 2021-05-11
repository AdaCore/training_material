-----------------
Basic Types Lab
-----------------

* Create types to handle the following concepts

   - Number of tests administered
   - Total of all test scores
   - Number of degrees in a circle
   - Collection of colors

* Create objects for the types you've created

   - Assign initial values to the objects
   - Print the values of the objects

* Modify the objects you've created and print the new values

    - Determine the average score for all the tests
    - Add 359 degrees to the initial circle value
    - Set the color object to the value right before the last possible value

-----------------------
Basic Types Lab Hints
-----------------------

* Understand the properties of the types

   - Do you need fractions or just whole numbers?
   - What happens when you want the number to wrap?

* Predefined package `Ada.Text_IO` is handy...

   - Procedure `Put_Line` takes a `String` as the parameter

* Remember attribute `'Image` returns a `String`

   .. code:: Ada

      <typemark>'image ( object )
      Object'image

----------------------------------------
Basic Types Lab Solution (Definitions)
----------------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Main is

      type Number_Of_Tests_T is range 0 .. 100;
      type Score_Total_T is digits 6 range 0.0 .. 10_000.0;

      type Degrees_T is mod 360;

      type Cymk_T is (Cyan, Magenta, Yellow, Black);

      Number_Of_Tests : Number_Of_Tests_T;
      Score_Total     : Score_Total_T;

      Angle : Degrees_T;

      Color : Cymk_T;
    
------------------------------------------
Basic Types Lab Solution (Implementation)
------------------------------------------
  
.. code:: Ada

   begin

      -- assignment
      Number_Of_Tests := 15;
      Score_Total     := 1_234.5;
      Angle           := 180;
      Color           := Magenta;

      Put_Line (Number_Of_Tests'Image);
      Put_Line (Score_Total'Image);
      Put_Line (Angle'Image);
      Put_Line (Color'Image);

      -- operations / attributes
      Score_Total := Score_Total / Score_Total_T (Number_Of_Tests);
      Angle       := Angle + 359;
      Color       := Cymk_T'Succ (Color);

      Put_Line (Score_Total'Image);
      Put_Line (Angle'Image);
      Put_Line (Color'Image);

   end Main;

--------------------------
Basic Types Extra Credit
--------------------------

* See what happens when your data is invalid / illegal

   - Number of tests = 0
   - Assign a very large number to the test score total
   - Color type only has one value
   - Add a number larger than 360 to the circle value

