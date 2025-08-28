========
Lab
========

----------------
Statements Lab
----------------

* Requirements

   - Create a simple program to build a time tracking sheet for the week
   
      + For every day of the week, print a line indicating the time of day (in hours)

   - Conditions for printing time

      + Only print times for "normal" working hours
      + No working on Sunday, so no times printed
      + Saturday is a half-day, so only print every other time slot

* Hints

   - Use a :ada:`for` loop to iterate over days of week and hours
   - Use a :ada:`case` statement to determine how start and end time
   - Use an :ada:`if` statement to determine if you are printing a time
   - For simplicity, feel free to use a 24-hour clock

-------------------------
Statements Lab Solution
-------------------------

.. container:: source_include 040_statements/lab/statements/answer/main.adb :code:Ada :number-lines:1
