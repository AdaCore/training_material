========
Lab
========

--------------------
Ada.Numerics Lab
--------------------
   
* Create an array

  * Size of at least 5
  * Element is a record containing

    * Integer counter
    * Floating point total

* Populate the array as follows

  * Loop some (large) number of times
  * For each iteration of the loop, pick a random element in the array
  * For the selected element

    * Increment the counter field
    * Find a random floating point number between 1_000 and 9_999
    * Add the square root of the number to the floating point field

* For each index in the array

  * Print out the array index
  * Print out the element counter
  * Print out the average of the square roots

* Observation

  * The more often you populate the array, the closer the averages *should* be to each other

----------------------------------------------
Ada.Numerics Lab Solution
----------------------------------------------

.. container:: source_include labs/answers/893_ada_numerics.txt :code:Ada :number-lines:1

.. include:: labs/893_ada_numerics.lab.rst

