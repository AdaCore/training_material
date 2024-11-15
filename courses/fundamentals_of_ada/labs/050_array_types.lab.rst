========
Lab
========

-----------
Array Lab
-----------

* Requirements

   - Create an array type whose index is days of the week and each element is a number
   - Create two objects of the array type, one of which is constant
   - Perform the following operations

      + Copy the constant object to the non-constant object
      + Print the contents of the non-constant object
      + Use an array aggregate to initialize the non-constant object
      + For each element of the array, print the array index and the value
      + Move part ("source") of the non-constant object to another part ("destination"), and then clear the source location
      + Print the contents of the non-constant object

* Hints

   - When you want to combine multiple strings (which are arrays!) use the concatenation operator (`&`)
   - Slices are how you access part of an array
   - Use aggregates (either named or positional) to initialize data

------------------
Arrays of Arrays
------------------

* Requirements

   - For each day of the week, you need an array of three strings containing names of workers for that day
   - Two sets of workers: weekend and weekday, but the store is closed on Wednesday (no workers)
   - Initialize the array and then print it hierarchically

-----------------------------------
Array Lab Solution - Declarations
-----------------------------------

.. container:: source_include labs/answers/050_array_types.txt :start-after:--Declarations :end-before:--Declarations :code:Ada :number-lines:1

-------------------------------------
Array Lab Solution - Implementation
-------------------------------------

.. container:: source_include labs/answers/050_array_types.txt :start-after:--Implementation :end-before:--Implementation :code:Ada :number-lines:15
