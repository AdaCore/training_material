=====
Lab
=====

----------------------
Pointer Programs Lab
----------------------

- Find the :filename:`120_pointer_programs` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place
   + Open a command prompt in that directory

- Windows: From the command line, run the :filename:`gpr_project_path.bat` file to set up your project path

   + The file resides in the :filename:`source` folder you installed
   + Pass in the version of SPARK you have installed (e.g. :command:`gpr_project_path 25.1`)
   + This only needs to be done once per command prompt window

.. note::

   For Linux users, the install location for SPARK varies greatly, so instead there is
   a shell script :filename:`gpr_project_path.sh` which gives you directions

- From the command-line, run :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-------------------
Swapping Pointers
-------------------

- Find and open the files :filename:`pointers.ads` and :filename:`pointers.adb` in :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` in flow analysis mode

- Fix the ownership error in :ada:`Swap_Ptr`

- Add postconditions to procedures :ada:`Swap` and :ada:`Swap_Ptr`

   + Hint: you cannot compare pointers in SPARK
   + Rerun :toolname:`GNATprove` to prove these procedures

-----------------------------
Allocation and Deallocation
-----------------------------

- Run :toolname:`GNATprove` to prove procedure :ada:`Realloc`

   + Understand the memory leak message and fix it.
   + Hint: you need to add a postcondition to :ada:`Dealloc`

- Understand what makes :ada:`Alloc` and :ada:`Dealloc` special

   + Discuss with the course instructor.

---------------------
Recursion and Loops
---------------------

- Review the rest of the code manipulating types :ada:`List_Cell` and :ada:`List_Acc`

   + Discuss with the course instructor.

- Run :toolname:`GNATprove` to prove the complete unit.

- Add a loop invariant in procedure :ada:`Init_List_Zero`

   + The postcondition of :ada:`Init_List_Zero` should be proved

- Add a loop variant in procedure :ada:`Init_List_Zero`

   + First using the structural loop variant
   + Next using a numerical loop variant, by defining a recusrive function
     :ada:`Length`

     |

     .. code:: Ada

        function Length
          (L : access constant List_Cell) return Big_Natural;
