========
Lab
========

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

-------------------------------
Using the "Prompts" Directory
-------------------------------

* Course material should have a link to a :filename:`Prompts` folder

* Folder contains everything you need to get started on the lab

  * :toolname:`GNAT Studio` project file :filename:`default.gpr`
  * Annotated / simplified source files

    * Source files are templates for lab solutions
    * Files compile as is, but don't implement the requirements
    * Comments in source files give hints for the solution

* To load prompt, either

  * From within :toolname:`GNAT Studio`, select :menu:`File` :math:`\rightarrow` :menu:`Open Project` and navigate to and open the appropriate :filename:`default.gpr` **OR**
  * From a command prompt, enter :command:`gnatstudio -P <full path to GPR file>`

    * If you are in the appropriate directory, and there is only one GPR file, entering :command:`gnatstudio` will start the tool and open that project

* These prompt folders should be available for most labs

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

      <typemark>'Image (Object)
      Object'Image   

--------------------------
Basic Types Extra Credit
--------------------------

* See what happens when your data is invalid / illegal

   - Number of tests = 0
   - Assign a very large number to the test score total
   - Color type only has one value
   - Add a number larger than 360 to the circle value

----------------------------------------
Basic Types Lab Solution - Declarations
----------------------------------------

.. container:: source_include labs/answers/030_basic_types.txt :start-after:--Declarations :end-before:--Declarations :code:Ada :number-lines:1

------------------------------------------
Basic Types Lab Solution - Implementation
------------------------------------------
  
.. container:: source_include labs/answers/030_basic_types.txt :start-after:--Implementation :end-before:--Implementation :code:Ada :number-lines:18

