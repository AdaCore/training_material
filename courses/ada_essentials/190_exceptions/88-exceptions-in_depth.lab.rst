========
Lab
========

-------------------------
Exceptions In-Depth Lab
-------------------------

(Simplified) Mathematical Operations Verification

  * Overview

    * Create an application that allows users to process an array of operations
      and track their success or failure

  * Goal

    * Application should allow user to add, subtract, multiply, and divide
    * All operations should be performed before any results are displayed

----------------------
Project Requirements
----------------------

* Exception Tracking

  * The following errors should be flagged

    * Input value out of range
    * Divide by zero
    * Constraint error

* Driver

  * :ada:`Main` program in the prompt contains example data that should
    verify all operations succeeding as well as each of the three failures

------------------------------------------------
Exceptions In-Depth Lab Solution - Main (Data)
------------------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/main.adb :start-after:main_data_begin :end-before:main_data_end :code:Ada :number-lines:1

------------------------------------------------------
Exceptions In-Depth Lab Solution - Main (Processing)
------------------------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/main.adb :start-after:main_processing_begin :end-before:main_processing_end :code:Ada :number-lines:32

------------------------------------------------------
Exceptions In-Depth Lab Solution - Operations (Spec)
------------------------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/operations.ads :code:Ada :number-lines:1

------------------------------------------------------
Exceptions In-Depth Lab Solution - Operations (Body)
------------------------------------------------------

.. container:: source_include 190_exceptions/lab/exceptions-in_depth/answer/operations.adb :code:Ada :number-lines:1
