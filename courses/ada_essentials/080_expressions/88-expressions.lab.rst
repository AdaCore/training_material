========
Lab
========

--------------------------
Expressions Lab
--------------------------

* Goal

  - Use expression functions to validate array contents

* Requirements

   - Prompt has three arrays of dates
   - For each set of dates, use *quantified expressions* to print True/False

      * If any date is not legal (taking into account leap years!)
      * If all dates are in the same calendar year

   - Use *expression functions* for all validation routines

* Hints

   - Use subtype membership for range validation
   - You will need *conditional expressions* in your functions
   - You *can* use component-based iterations for some checks

      * But you *must* use indexed-based iterations for others

--------------------------------------------
Expressions Lab Solution - Checks
--------------------------------------------

.. container:: source_include 080_expressions/lab/expressions/answer/main.adb :start-after:checks_begin :end-before:checks_end :code:Ada :number-lines:4

------------------------------------------
Expressions Lab Solution - Main
------------------------------------------

.. container:: source_include 080_expressions/lab/expressions/answer/main.adb :start-after:main_begin :end-before:main_end :code:Ada :number-lines:37
