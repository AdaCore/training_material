--------------------------------
Discriminated Record Types Lab
--------------------------------
   
(Simplified) University Class Scheduling System

  * Overview

    * Four categories of people on campus

      Undergrad Student
        Takes some classes

      Grad Student
        Takes some classes and helps teach some classes

      Professor
        Teaches some classes

      Other
        Anybody else

  * Goal

    * Create application that allows user to:

      * Identify a person by first name, last name, and category
      * Maintain a list of classes attended by students
      * Maintain a list of classes taught by Grad Students and Professors
      * Print a list of people on campus and the classes they take/teach

----------------------
Project Requirements
----------------------

* Variant Records

  * Types must be :ada:`private`
  * Text content must be stored using your own variant record data type (no :ada:`Ada.Strings.*`)

    * This means you will need to implement some basic operations, such as concatenation

  * You must use the same record for every person, and you must not be able to make invalid assignments

    * For example, your code must prevent an Undergrad from teaching a class through run-time checks (not validation)

* Driver

  * You can read data from a file, the console, or you can hard-code the calls
  * Your testing should show your code flags incorrect scenarios

    * Even if that's just an exception propagated out of the executable

--------------------------------
Extra Credit (If Time Permits)
--------------------------------

* **After** your testing shows failure for an incorrect scenario, try adding validation routines

  * If you do it first, you won't know what happens if your validation doesn't catch every issue
  * Good software practice: Fix the problem *before* you prevent the problem

* Print list in a specific order (for example: by name, by category)

* Use generics for data entry formatting/verification

----------------------------------------------------------
Discriminated Record Types Lab Solution - Strings (Spec)
----------------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Strings_Spec :end-before:--Strings_Spec :code:Ada

-------------------------------------------------------------
Discriminated Record Types Lab Solution - Scheduling (Spec)
-------------------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Scheduling_Spec :end-before:--Scheduling_Spec :code:Ada

-------------------------------------------------
Discriminated Record Types Lab Solution - Main
-------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Main :end-before:--Main :code:Ada

----------------------------------------------------------
Discriminated Record Types Lab Solution - Strings (Body)
----------------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Strings_Body :end-before:--Strings_Body :code:Ada

-------------------------------------------------------------
Discriminated Record Types Lab Solution - Scheduling (Body)
-------------------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Scheduling_Body :end-before:--Scheduling_Body :code:Ada

