========
Lab
========

-------------------------
Exceptions In-Depth Lab
-------------------------

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

------------------------------------------------------
Exceptions In-Depth Lab Solution - Calculator (Spec)
------------------------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/calculator.ads :code:Ada :number-lines:1

-----------------------------------------
Exceptions In-Depth Lab Solution - Main
-----------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/main.adb :code:Ada :number-lines:1

------------------------------------------------------
Exceptions In-Depth Lab Solution - Calculator (Body)
------------------------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/calculator.adb :code:Ada :number-lines:1

------------------------------------------
Exceptions In-Depth Lab Solution - Debug
------------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/debug_pkg.ads :code:Ada :number-lines:1

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/debug_pkg.adb :code:Ada :number-lines:1

