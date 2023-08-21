-----------------
Elaboration Lab
-----------------

* Requirements

   - Create a `pure` package containing some constants

      + Lower limit of some integer range
      + Upper limit of some integer range
      + Flag indicating an invalid state

   - Create a package whose interface consists solely of one global object

      + Array of integers initialized to the invalid state

   - During elaboration, fill in the array object by querying the user

      + All entries must be in the range of *Lower Limit* to *Upper Limit*

   - Create a `main` program to print out the array

      + Only print values set by the user

* Hints

   - The only indication of actual number of entries is the array itself
   - Need to tell the compiler that the global object is initialized in the package body

---------------------------------------
Elaboration Lab Solution - Constants
---------------------------------------

.. container:: source_include labs/answers/spec_880_elaboration.txt :start-after:--Constants :end-before:--Constants :code:Ada :number-lines:1

---------------------------------------
Elaboration Lab Solution - Data Store
---------------------------------------

.. container:: source_include labs/answers/spec_880_elaboration.txt :start-after:--Datastore :end-before:--Datastore :code:Ada :number-lines:1

---------------------------------
Elaboration Lab Solution - Main
---------------------------------

.. container:: source_include labs/answers/spec_880_elaboration.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
