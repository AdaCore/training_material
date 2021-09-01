----------------
Contracts Lab
----------------

- Find the :filename:`060_contracts` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane

.. container:: speakernote


   Each step in the lab will probably be 10-15 minutes - lab should take at least 45 minutes

-----------------------
Longest Common Prefix
-----------------------

* Requirements

   + Given a global array A containing integers
   + Given two integers X and Y, indices of A
   + Return the length of the longest common prefix of the two sub-arrays starting at X and Y.

* Example

   .. list-table::
      :width: 90%

    * - 3

      - 4
      - 5
      - 0
      - 9
      - 4
      - 5
      - 0
      - 9
      - 2

* ``{X = 2, Y = 6} => R = 4``

--------------------
Lab Tasks Overview
--------------------

- The algorithm is implemented (it may have bugs), see the exercise directory.
- We are going to write contracts
- We are going to debug those contracts
- We are going to check those contracts with :toolname:`GNATprove`

---------------------
Lab Precondition(s)
---------------------

* From the requirements:

   - Given two integers X and Y, indices of A

* Write the precondition on LCP that corresponds to this statement.
* Write tests in :filename:`main.adb` that show that this precondition seems to fail and succeed when expected.
* Use :toolname:`GNATprove` (:menu:`SPARK` |rightarrow| :menu:`Prove File`) to demonstrate that the precondition is indeed verified and not verified depending on the test.

   - **NOTE:** If the file fails to prove, you might need to increase the **proof level**

.. container:: speakernote


   The proof issue doesn't have anything to do with this precondition because the "while" loop protects against bad indices
   A good set of values would be X = 2, Y = 7, result = 4

--------------------
Lab Contract Cases
--------------------

* From the requirements, we can set the following conditions:

   - If A (X) /= A (Y), the result is 0.
   - If X = Y, the result is the length from X to last.
   - Otherwise, the result is above 0.

* Write the contract cases for the above
* Run tests in main.adb to verify that these contracts seem correct
* Use :toolname:`GNATprove` to prove the contract cases

.. container:: speakernote


   For the contract cases, we need the precondition we set up before (otherwise a(x) and a(y) may be invalid)

----------------------
Lab Postcondition(s)
----------------------

* From the requirements, we can set the following postcondition:

   + All the values of A in the range starting from X with the length of LCP'Result are equal to the values of A in the range starting from Y with the length of LCP'Result.

* Write the postcondition for the above
* Run tests in :filename:`main.adb` to verify that this postcondition seems correct
* Use :toolname:`GNATprove` to prove the postcondition

----------------------------------------
Lab Postcondition - Full Specification
----------------------------------------

* From the requirements, one part of the postcondition is missing, the fact that the prefix is indeed the biggest one:

   + The element at X + LCP'Result is different from the one at Y + LCP'Result, unless we have reached beyond A'Last.

* Complete your postcondition for the above
* Run tests in :filename:`main.adb` to verify that this postcondition seems correct
* Use :toolname:`GNATprove` to prove the postcondition
