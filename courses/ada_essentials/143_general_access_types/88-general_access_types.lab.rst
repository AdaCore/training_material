========
Lab
========

--------------------------
General Access Types Lab
--------------------------
   
* We want to see the distribution of values when totaling the roll of three dice

  * Show the number of times a value comes up in value order
  * Show the number of times a value comes up in frequency order

* The prompt contains

  * Main program that rolls the dice
  * Framework for :ada:`Database` that will

    * Maintain a count of the number of times a particular total appears
    * Increment the counter based on totaling the roll of three dice
    * Return a pointer to the count for a particular total
    * Print the count for each total in order

  * Framework for :ada:`Table` that will print the totals in frequency order

    * Frequency order means the most common roll first, next most common second, etc.
      (or least to most)

-------
Hints
-------

* :ada:`Database` needs

  * Subprogram to convert the three dice to a single value
  * Array of counts, one for each possible value (and these counts will need to be directly
    visible to the outside world)
  
* :ada:`Table` needs

  * Array that keeps track of values and the count for each value (without asking for the
    count each time)
  * Routine to sort array by count

-------------------------
Lab Solution - Database
-------------------------

.. container:: source_include 143_general_access_types/lab/general_access_types/answer/database.ads :code:Ada :number-lines:1

.. container:: source_include 143_general_access_types/lab/general_access_types/answer/database.adb :code:Ada :number-lines:1

----------------------
Lab Solution - Table
----------------------

.. container:: source_include 143_general_access_types/lab/general_access_types/answer/table.adb :code:Ada :number-lines:1
