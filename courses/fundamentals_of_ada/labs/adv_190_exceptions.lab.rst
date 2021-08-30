----------------
Exceptions Lab
----------------

(Simplified) Calculator

  * Overview

    * Create an application that allows users to enter a simple calculation and get a result

  * Goal

    * Application should allow user to add, subtract, multiply, and divide
    * We want to track exceptions without actually "interrupting" the application
    * When the user has finished entering data, the application should report the errors found

----------------------
Project Requirements
----------------------

* Exception Tracking

  * Input errors should be flagged (e.g. invalid operator, invalid numbers)
  * Divide by zero should be it's own special case exception
  * Operational errors (overflow, etc) should be flagged in the list of errors

* Driver

  * User should be able to enter a string like "1 + 2" and the program will print "3"
  * User should not be interrupted by error messages
  * When user is done entering data, print all errors (raised exceptions)

* Extra Credit

  * Allow multiple operations on a line

---------------------------------------------
Exceptions Lab Solution - Calculator (Spec)
---------------------------------------------

.. container:: source_include labs/answers/adv_190_exceptions.txt :start-after:--Calculator_Spec :end-before:--Calculator_Spec :code:Ada

--------------------------------
Exceptions Lab Solution - Main
--------------------------------

.. container:: source_include labs/answers/adv_190_exceptions.txt :start-after:--Main :end-before:--Main :code:Ada

---------------------------------------------
Exceptions Lab Solution - Calculator (Body)
---------------------------------------------

.. container:: source_include labs/answers/adv_190_exceptions.txt :start-after:--Calculator_Body :end-before:--Calculator_Body :code:Ada

---------------------------------
Exceptions Lab Solution - Debug
---------------------------------

.. container:: source_include labs/answers/adv_190_exceptions.txt :start-after:--Debug :end-before:--Debug :code:Ada

