-------------------
Proving AoRTE Lab
-------------------

- Find the :filename:`090_proving_absence_of_run_time_errors` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane

.. container:: speakernote


   Lab should take about 30 minutes
   Extra credit adds about 15 minutes

----------------------------------
Run-Time Exception Freedom Proof
----------------------------------

* Find and open the file :filename:`main.adb`

* Study the function `Is_Right_Angle_Triangle`

   + Function is intended to determine if given triangle (specified by lengths of its three sides) is a right-angle triangle
   + Postcondition and the code have been made simpler by passing as first argument the longest side

      - **Note:** The function could have been implemented as a simple expression function.

* Use the SPARK tools to attempt to prove RTE freedom for this function

   + Using the default settings in :filename:`default.gpr`
   + The tools should find potential overflows in the code.

* The body of the main program contains a test to provoke the run-time exception(s)

   + Compile and run the program to check that an exception is raised.

.. container:: speakernote


   Implicit precondition - 3 sides can form a triangle

-----------------------------------------
Run-Time Exception Freedom Proof (cont)
-----------------------------------------

* Strengthen the precondition

   + There is an implicit precondition, what is it?
   + Square of longest side is in the range of `Natural`
   + Sum of the squares of the other two sides is in the range of` Natural`

* Rebuild and re-run.

   + The result should be identical

      - Exception is raised in the body of the function (not in the precondition itself).

* Enable run-time assertion checks

   + :menu:`Edit` |rightarrow| :menu:`Project Properties...` |rightarrow| :menu:`Switches` |rightarrow| :menu:`Ada`
   + Tick **Enable Assertions**
   + Clean, rebuild and re-run.

      - This time the precondition should be checked at run time and an exception should be raised.

-----------------------------------------
Run-Time Exception Freedom Proof (cont)
-----------------------------------------

* Use the :menu:`Prove` command to check for run-time errors.

   + It should report a potential overflow in the precondition

      - But not in the code or the postcondition -  why?

* Again, modify the Ada switches in the project properties

   + Tick :menu:`Overflow checking` (under **Run-time checks**)

      - **Note:** Compiler switch :command:`-gnato` appears in the switch list at the bottom of the project settings window.

   + Change :command:`-gnato` to :command:`-gnato13` to eliminate the possibility of overflow in the contract

* Re-prove

   + Now the tools should prove the contract itself
   + But fail to prove the test case which calls the function with values that fail the precondition check.

-----------------------------------------
Run-Time Exception Freedom Proof (cont)
-----------------------------------------

* Remove the precondition from the contract
* Change the switch to :command:`-gnato33` to eliminate the possibility of overflow in the code and the contract
* Re-prove, rebuild (clean or tick **Recompile if switches changed**) and re-run.

   + What are the pros and cons of using this approach?

----------------------------------------------
Run-Time Exception Freedom Proof Extra Credit
----------------------------------------------

* Extend the program

   + Given the lengths of the three sides, detect the type of triangle

      - **right angle**, **equilateral**, **isosceles**, **other**, **not a triangle**

   + Solution should include suitable contracts, which should be proved by the tools.

* Execute a set of suitable test cases to check the program's operation.
