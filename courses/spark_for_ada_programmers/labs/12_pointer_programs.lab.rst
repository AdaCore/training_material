----------------------
Pointer Programs Lab
----------------------

- Find the :filename:`12_pointer_programs` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- Define variable :code:`SPARKLIB_OBJECT_DIR` to have value :code:`obj` in the environment

  - For example with bash/zsh: :command:`export SPARKLIB_OBJECT_DIR=obj`

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-------------------
Swapping Pointers
-------------------

- Find and open the files :filename:`pointers.ads` and :filename:`pointers.adb` in :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` in flow analysis mode

- Fix the ownership error in :code:`Swap_Ptr`

- Add postconditions to procedures :code:`Swap` and :code:`Swap_Ptr`

  + Hint: you cannot compare pointers in SPARK
  + Rerun :toolname:`GNATprove` to prove these procedures

-----------------------------
Allocation and Deallocation
-----------------------------

- Run :toolname:`GNATprove` to prove procedure :code:`Realloc`

  + Understand the memory leak message and fix it.
  + Hint: you need to add a postcondition to :code:`Dealloc`

- Understand what makes :code:`Alloc` and :code:`Dealloc` special

  + Discuss with the course instructor.

---------------------
Recursion and Loops
---------------------

- Review the rest of the code manipulating types :code:`List_Cell` and :code:`List_Acc`

  + Discuss with the course instructor.

- Run :toolname:`GNATprove` to prove the complete unit.

- Add a loop invariant in procedure :code:`Init_List_Zero`

  + The postcondition of :code:`Init_List_Zero` should be proved

- Add a loop variant in procedure :code:`Init_List_Zero`

  + First using the structural loop variant
  + Next using a numerical loop variant, by defining a recusrive function
    :code:`Length`

    .. code:: Ada

       function Length (L : access constant List_Cell) return Big_Natural;