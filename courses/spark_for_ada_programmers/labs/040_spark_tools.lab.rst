=====
Lab
=====

.. |rightarrow| replace:: :math:`\rightarrow`

-----------------
SPARK Tools Lab
-----------------

- Find the :filename:`040_spark_tools` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

----------
Overview
----------

This lab is designed to show you how to use the SPARK toolchain in :toolname:`GNAT Studio` 

  * We'll use phrases you might not know yet, but when you use this
    lab as reference, you'll understand

The goals of this lab are

  * Perform Flow Analysis
  * Run the prover on some code
  * Understand the output from those processes

.. warning::

   If your :toolname:`GNAT Studio` does not have a :menu:`SPARK` menu item,
   then SPARK is not on your path. You either need to install SPARK, or add
   it to your PATH environment variable

----------------------------
Step 1 - Ensure SPARK Mode
----------------------------

Try to compile your source file

1. Open :filename:`linear_search.adb`
2. Compile your file either using the *Build target Compile File* icon or 
   :menu:`Build` |rightarrow| :menu:`Compile File`

Note the error message:

   :color-red:`error: function cannot have parameter of mode "out" or "in out"`

SPARK does not allow :ada:`out` or :ada:`in out` function parameters. Fix the problem
by commenting out the :ada:`Non_SPARK_Search` spec and body and recompiling.

-----------------------------------
Step 2 - Performing Flow Analysis
-----------------------------------

*Flow Analysis* is the verification that data is initialized properly. We
check this from the :menu:`SPARK` menu:

1. Select either the spec or body file for :ada:`Linear_Search`
2. Perform flow analysis by running :menu:`SPARK` |rightarrow| :menu:`Examine File`

Note the error message:

   :color-red:`high: "Res.At_Index" is not initialized`

We have found a path where :ada:`Res.Index` is not initialized. Correct it by
setting it to some value, and then perform the analysis again

----------------------------
Step 3 - Proving Your Code
----------------------------

To use :toolname:`GNATprove` to verify the correct processing, you
need to use one of the :menu:`Prove` options.

1. Select either the spec or body file for :ada:`Linear_Search`
2. Call the prover by running :menu:`SPARK` |rightarrow| :menu:`Prove File`

Note the error message:

   :color-red:`medium: postcondition might fail`

   :color-red:`cannot prove A (SPARK_Search'Result.At_Index) = Val`

The prover is failing because analysis shows that the code does not
match the postcondition. Try fixing the problem and running the
prover again.

   (Page down once for a hint, a second time for the answer!)

.. container:: animate 2-

   *Hint: The code is correct*

.. container:: animate 3-

   We want to verify the location if the result **is** found - remove
   the :ada:`not` from the postcondition
