========
Lab
========

-----------------
Overloading Lab
-----------------

* Requirements

   - Create multiple functions named :ada:`Convert` to convert between a character digits and its name

      + One routine should take a digit and return the name (e.g. **'3'** would return **three**)

      + One routine should take the name and return the digit (e.g. **two** would return **'2'**)

      + Hint: enumerals for the name will be easier than dealing with strings

   - Create overloaded addition functions that will add any combination of the two (digit and name)

      + The result can be an integer (i.e. dont worry about converting the result of :ada:`'7' + eight`)

      + Hint: It might be easier to convert one to the other before adding!

   - The prompt has four equations in the comments - use those to prove your code works

-------------------------------------------------
Overloading Lab Solution - Conversion Functions
-------------------------------------------------

.. container:: source_include 090_overloading/lab/overloading/answer/main.adb :start-after:conversions_begin :end-before:conversions_end :code:Ada :number-lines:5

--------------------------------------
Overloading Lab Solution - Operators
--------------------------------------

.. container:: source_include 090_overloading/lab/overloading/answer/main.adb :start-after:operators_begin :end-before:operators_end :code:Ada :number-lines:32

-------------------------------------------------
Overloading Lab Solution - Main
-------------------------------------------------

.. container:: source_include 090_overloading/lab/overloading/answer/main.adb :start-after:main_begin :end-before:main_end :code:Ada :number-lines:79
