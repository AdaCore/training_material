========
Lab
========

-----------------
Overloading Lab
-----------------

* Requirements

   - Create multiple functions named "Convert" to convert between digits and text representation

      + One routine should take a digit and return the text version (e.g. **3** would return **three**)

      + One routine should take text and return the digit (e.g. **two** would return **2**)

   - Query the user to enter text or a digit and print its equivalent
   - If the user enters consecutive entries that are equivalent, print a message

      + e.g. **4** followed by **four** should get the message

* Hints

   - You can use enumerals for the text representation

      + Then use *'Image* / *'Value* where needed

   - Use an equivalence function to compare different types

-------------------------------------------------
Overloading Lab Solution - Conversion Functions
-------------------------------------------------

.. container:: source_include labs/answers/090_overloading.txt :start-after:--Conversion_Functions :end-before:--Conversion_Functions :code:Ada :number-lines:4

-------------------------------------------------
Overloading Lab Solution - Main
-------------------------------------------------

.. container:: source_include labs/answers/090_overloading.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:40
