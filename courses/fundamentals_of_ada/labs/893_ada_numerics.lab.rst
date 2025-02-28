=======
Lab
=======

--------------------
Ada.Numerics Lab
--------------------
   
* Create an array

  * Size of at least 5
  * Component is a record containing

    * Integer counter
    * Floating point total

* Populate the array as follows

  * Loop some (large) number of times
  * For each iteration of the loop, pick a random component in the array
  * For the selected component

    * Increment the counter component
    * Find a random floating point number between 1_000 and 9_999
    * Add the square root of the number to the floating point component

* For each index in the array

  * Print out the array index
  * Print out the component counter
  * Print out the average of the square roots

* Observation

  * The more often you populate the array, the closer the averages *should* be to each other

----------------------------------------------
Ada.Numerics Lab Solution
----------------------------------------------

.. container:: source_include labs/answers/893_ada_numerics.txt :code:Ada :number-lines:1

