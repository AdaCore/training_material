----------------
Statements Lab
----------------

* Requirements

   - Create a simple algorithm to count number of hours worked in a week

      + Use `Ada.Text_IO.Get_Line` to ask user for hours worked on each day
      + Any hours over 8 gets counted as 1.5 times number of hours (e.g. 10 hours worked will get counted as 11 hours towards total)
      + Saturday hours get counted at 1.5 times number of hours
      + Sunday hours get counted at 2 times number of hours

   - Print total number of hours "worked"

* Hints

   - Use `for` loop to iterate over days of week
   - Use `if` statement to determine overtime hours
   - Use `case` statement to determine weekend bonus

-----------------------------
Statements Lab Extra Credit
-----------------------------

* Use an inner loop when getting hours worked to check validity

   - Less than 0 should exit outer loop
   - More than 24 should not be allowed

-------------------------
Statements Lab Solution
-------------------------

.. container:: source_include labs/answers/040_statements.txt :start-after:--Statements :end-before:--Statements :code:Ada
