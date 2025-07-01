=============
Assumptions
=============

-----------------------------
Quiz - Implicit Assumptions
-----------------------------

Is the following code correct?

.. code:: ada

   package Random_Numbers
     with SPARK_Mode
   is
      function Random (From, To : Integer) return Integer
        with Post => Random'Result in From .. To;
   private
      pragma SPARK_Mode (Off);
      ...

.. container:: animate

   * No - :toolname:`GNATprove` assumes that :ada:`Random` is a mathematical
     function

     - An abstract state should be added in package :ada:`Random_Numbers`
     - :ada:`Random` should be a procedure
     - A data dependency contract should be added for reads/writes to this
       abstract state

   * No - :toolname:`GNATprove` assumes that the postcondition of :ada:`Random`
     is always satisfied, even when :ada:`From > To`

     - A precondition :ada:`From <= To` should be added
     - The implementation must satisfy the postcondition

------------------
Tool Assumptions
------------------

* Results of flow analysis and proof are valid under assumptions

  - About the system behavior as modelled in SPARK
  - About parts of the code not in SPARK
  - About the hardware platform

* All assumptions should be reviewed and validated

  - Complete list in SPARK User's Guide section 7.3.7

* Common assumptions whether or not complete program in SPARK

* Additional assumptions

  - When only part of the program in SPARK
  - When :toolname:`GNATprove` never called with all bodies available
  - When code not compiled with GNAT

