****************
Exceptions Lab
****************

=====
Lab
=====

-------------
Instructions
-------------

* Requirements

   - Create a simple math package that performs *square*, *square root*, *multiply*, *divide*

      + *square* and *square root* should raise a user-defined exception if their input value is out of range
      + *multiply* and *divide* should not do any range checking

   - Your main program should call the four functions with various values

      + If the user-defined exception is caught, print whatever message was raised
      + If a pre-defined exception is caught, print all available exception information

* Hints

   - Define a floating point type with a range that includes negative numbers

      + This allows illegal values for *square root*

   - Use `Ada.Numerics.Generic_Elementary_Functions` for the *square root* function

--------------------------------
Exceptions Lab Solution - Math 
--------------------------------

.. container:: source_include labs/answers/190_exceptions.txt :start-after:--Math :end-before:--Math :code:Ada

---------------------------------------------------
Exceptions Lab Solution - Main (Test Subprograms)
---------------------------------------------------

.. container:: source_include labs/answers/190_exceptions.txt :start-after:--Main :end-before:--Main :code:Ada
