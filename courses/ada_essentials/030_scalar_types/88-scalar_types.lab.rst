=====
Lab
=====

------------------
Scalar Types Lab
------------------

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

------------------------
Scalar Types Lab Hints
------------------------

* Understand the properties of the types

   - Do you need fractions or just whole numbers?
   - What happens when you want the number to wrap?

* Predefined package :ada:`Ada.Text_IO` is handy...

   - Procedure :ada:`Put_Line` takes a :ada:`String` as the parameter

* Remember attribute :ada:`'Image` returns a :ada:`String`

  .. code:: Ada

     <typemark>'Image (Object)
     Object'Image

------------------------------------------
Scalar Types Lab Solution - Declarations
------------------------------------------

.. container:: source_include 030_scalar_types/lab/scalar_types/answer/main.adb :start-after:--Declarations :end-before:--Declarations :code:Ada :number-lines:1

--------------------------------------------
Scalar Types Lab Solution - Implementation
--------------------------------------------

.. container:: source_include 030_scalar_types/lab/scalar_types/answer/main.adb :start-after:--Implementation :end-before:--Implementation :code:Ada :number-lines:18

----------------------------------------------
Scalar Types Extra Credit - Compiler Warning
----------------------------------------------

* Set :ada:`Number_Of_Tests` to ``0``

  * Does it compile?
  * Does it run?

  .. code:: Ada
    :font-size: small
    :number-lines: 35

    Test_Score_Total := Test_Score_Total /
                        Test_Score_Total_T (Number_Of_Tests);

.. container:: animate 2-

  **Compile warning**

  .. code:: error
    :font-size: small

    main.adb:35:43: warning: division by zero

.. container:: animate 3-

  **Runtime error**

  .. code:: error
    :font-size: small

    raised CONSTRAINT_ERROR : main.adb:35 divide by zero

-------------------------------------------------------
Scalar Types Extra Credit - Invalid Static Expression
-------------------------------------------------------

* Set :ada:`Cmyk_T` to contain only one enumeral

  .. code:: Ada
    :number-lines: 37

    Color := Cmyk_T'Pred (Cmyk_T'Last);

.. container:: animate 2-

  .. code:: error
    :font-size: small

    main.adb:37:30: error: Pred of "Cmyk_T'First"
    main.adb:37:30: error: static expression fails Constraint_Check

--------------------------------------------
Scalar Types Extra Credit - Compiler Error
--------------------------------------------

* Add number larger than 360 to :ada:`Angle`

  .. code:: Ada
    :number-lines: 36

    Angle := Angle + 459;

.. container:: animate 2-

  .. code:: error
    :font-size: scriptsize

    main.adb:36:32: error: value not in range of type "Degrees_T" defined at line 8
