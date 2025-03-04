--------------------------
:toolname:`GNATstub` Lab
--------------------------

* We are going implement a simple math package that does addition and subtraction

   * The exectuable takes 3 numbers on the command line - adds the first two, subtracts the third, and prints the result

* Copy the :filename:`100_gnatstub` lab folder from the course materials location

* Contents of the folder:

  * :filename:`default.gpr` - project file
  * :filename:`main.adb` - main program
  * :filename:`math.ads` - package spec that we want to implement

.. note:: We use animation - if you don't know the answer, Page Down should give it to you

----------------------
Build the Executable 
----------------------

1. Open a command prompt window and navigate to the directory containing :filename:`default.gpr`

2. Try to build the exectuable (:command:`gprbuild -P default.gpr`)

   * Build fails because :ada:`Math` is not implemented

3. Build a stub for :ada:`Math`

   * Make sure you copy the file header comment into the stub

.. container:: animate 2-

   :command:`gnatstub --comment-header-spec math.ads`

----------------------------
Build the Executable Again
----------------------------

1. Build the executable again

   * Builds, but you get compile warnings from the stubbed subprograms

2. Run the executable

   * Remember to add three numbers on the command line

3. Executable should fail with :ada:`Program_Error` in :ada:`Add_Two_Numbers`

   * Default stub behavior

4. Rebuild the stub without exceptions and run it again

.. container:: animate 2-

   :command:`gnatstub -f --comment-header-spec --no-exception math.ads`

   * Exception now raised in :ada:`Subtract_Two_Numbers`

      * Exceptions always raised for functions in a stub

-----------------------
Implement :ada:`Math`
-----------------------

1. Edit the :ada:`Math` package body to implement the two subprograms

2. Build and run the executable

--------------------------
:ada:`Math` Package Body
--------------------------

.. container:: source_include labs/100_gnatstub/answer/math.adb
