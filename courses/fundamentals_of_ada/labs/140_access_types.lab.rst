------------------
Access Types Lab
------------------

* Requirements

   - Create a datastore containing an array of records

      * Each record contains an array to store strings
      * Interface to the array consists *only* of functions that return an element of the array (Input parameter would be the array index)

   - Main program should allow the user to specify an index and a string

      + String gets appended to end of string pointer array
      + When data entry is complete, print only the elements of the array that have data

* Hints

   - Interface functions need to pass back pointer to array element

      + For safety, create a function to return a modifiable pointer and another to return a read-only pointer

   - Cannot create array of variable length strings, so use pointers

---------------------------------------
Access Types Lab Solution - Datastore
---------------------------------------

.. container:: source_include labs/answers/140_access_types.txt :start-after:--Datastore :end-before:--Datastore :code:Ada :number-lines:1

----------------------------------
Access Types Lab Solution - Main
----------------------------------

.. container:: source_include labs/answers/140_access_types.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
