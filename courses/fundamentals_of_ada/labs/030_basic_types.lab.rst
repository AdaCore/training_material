-----------------
Basic Types Lab
-----------------

* Create types to handle the following concepts

   - Determining average test score

      - Number of tests taken
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

      <typemark>'image ( Object )
      Object'image

----------------------------------------
Basic Types Lab Solution - Declarations
----------------------------------------

.. container:: source_include labs/answers/030_basic_types.txt :start-after:--Declarations :end-before:--Declarations :code:Ada

------------------------------------------
Basic Types Lab Solution - Implementation
------------------------------------------
  
.. container:: source_include labs/answers/030_basic_types.txt :start-after:--Implementation :end-before:--Implementation :code:Ada

--------------------------
Basic Types Extra Credit
--------------------------

* See what happens when your data is invalid / illegal

   - Number of tests = 0
   - Assign a very large number to the test score total
   - Color type only has one value
   - Add a number larger than 360 to the circle value

