--------------------
SPARK Boundary Lab
--------------------

- Find the :filename:`15_spark_boundary` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-----------------
System Boundary
-----------------

- Find and open the files :filename:`alarm.ads` and :filename:`alarm.adb` in
  :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` on the unit

  + Check that you understand the error messages.

- Specify correct volatility properties for :code:`Temperature` and
  :code:`Status`

  + :code:`Temperature` is an input register
  + :code:`Status` is an output port

- Rerun :toolname:`GNATprove` on the unit

  + Fix the SPARK violations in the implementation
  + Hint: you need to mark one of the functions as a volatile function

- Add an external state :code:`State` with both :code:`Temperature` and
  :code:`Status` as constituents

  + What is the problem?

- Add separate external states with suitable volatile properties for
  :code:`Temperature` and :code:`Status`

  + The unit should be fully proved

- Review warnings and mark variables with aspect :code:`Warnings => Off`

-------------------
Software Boundary
-------------------

- Find and open the files :filename:`random_numbers.ads` and :filename:`random_numbers.adb` in
  :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` on the unit

  + Check that you understand the error message.

- Add aspect :code:`SPARK_Mode` to the package body with value :code:`Off`

- Run :toolname:`GNATprove` on the unit

  + Check that there are no messages.
  + Is the spec compatible with SPARK?

- Complete the spec so that it is compatible with SPARK

-----------------------------
Integration With C and Rust
-----------------------------

- Find and open the file :filename:`main.adb` in :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` on the unit

  + Fix the warnings with suitable annotations on the declaration of :code:`Swap`

- Add a suitable postcondition on :code:`Swap`

  + Check that you can prove after the call that the values of :code:`X` and
    :code:`Y` have been swapped
  + Hint: add a suitable assertion

- Compile the code of :filename:`main.adb`

  .. code:: console

     gcc -c main.adb

- Compile a C implementation for swap in :filename:`swap.c`, link it with the
  SPARK code, and run the executable

  .. code:: console

     gcc -c swap.c
     gnatbind main
     gnatlink main swap.o
     ./main

- Compile a Rust implementation for swap in :filename:`swap.rs`, link it with the
  SPARK code, and run the executable

  .. code:: console

     rustc --crate-type=lib --emit=obj swap.rs
     gnatbind main
     gnatlink main swap.o
     ./main

- What assumptions did you make on C or Rust implementations?

  + Discuss these with the course instructor.
