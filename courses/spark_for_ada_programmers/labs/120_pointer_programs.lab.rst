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

-------------------------
Swapping Pointers (1/2)
-------------------------

.. container:: animate 1-

   - Find and open the files :filename:`pointers.ads` and :filename:`pointers.adb` in :toolname:`GNAT Studio`

      - Run :menu:`SPARK` |rightarrow| :menu:`Examine File`

.. container:: animate 2-

   :color-red:`pointers.ads:11:14: error: return from "Swap_Ptr" with moved value for "X"`

   :color-red:`pointers.adb:16:1: error: object was moved at pointers.adb:16 [E0010]`

   :color-red:`pointers.ads:11:14: error: launch "gnatprove --explain=E0010" for more information`

   - Run the suggested :toolname:`GNATprove` command to see what help is available

   - Fix the ownership error in :ada:`Swap_Ptr`

.. container:: animate 3-

   Hint: The code actually has a bug, which is what is causing the error

.. container:: animate 4-

   .. code:: Ada

      procedure Swap_Ptr (X, Y : in out not null Int_Acc) is
         Tmp : Int_Acc := X;
      begin
         X := Y;
         Y := Tmp;
      end Swap_Ptr;

-------------------------
Swapping Pointers (2/2)
-------------------------

.. container:: animate 1-

   - Add postconditions to procedures :ada:`Swap` and :ada:`Swap_Ptr`
   - Run :menu:`SPARK` |rightarrow| :menu:`Prove Subprogram` for each of these subprograms

     -  Select :menu:`Report checks proved` option to verify postconditions proved

.. container:: animate 2-

   *Hint: you cannot compare pointers in SPARK*

.. container:: animate 3-

   .. code:: Ada

      procedure Swap (X, Y : not null Int_Acc)
        with Post => X.all = Y.all'Old and then Y.all = X.all'Old;

      procedure Swap_Ptr (X, Y : in out not null Int_Acc)
        with Post => X.all = Y.all'Old and then Y.all = X.all'Old;

-----------------------------
Allocation and Deallocation
-----------------------------

.. container:: animate 1-

   - Run :menu:`SPARK` |rightarrow| :menu:`Prove Subprogram` for :ada:`Realloc`

      + Select :menu:`Report checks proved` option to show all proofs
      + Understand the memory leak message and fix it.

.. container:: animate 2-

   *Hint: you need to add a postcondition to* :ada:`Dealloc` *so the prover*
   *knows that you are not overwriting a pointer*

.. container:: animate 3-

   .. code:: Ada

      procedure Dealloc (X : in out Int_Acc)
      with Depends => (X => null, null => X),
           Post => X = null;

   *Note the message verifying no memory leak*

   :color-red:`pointers.adb:29:9: info: absence of resource or memory leak proved`

---------------------
Recursion and Loops
---------------------

.. container:: animate 1-

   - Examine :ada:`List_Cell` and :ada:`List_Acc` and the subprograms that use them

      - Comments in code should be enough documentation

   - Run :menu:`SPARK` |rightarrow| :menu:`Prove File`

.. container:: animate 2-

   :color-red:`pointers.ads:47:19: medium: postcondition might fail`

   - Add :ada:`Loop_Invariant` to help prover verify postcondition

      - Hint: as we traverse the list, we want to check the values in
        the list match the values of the borrowed pointer when we
        are done with the borrow

.. container:: animate 3-

   .. code:: Ada

      while B /= null loop
         pragma Loop_Invariant
           (if All_List_Zero (At_End (B)) then All_List_Zero (At_End (L)));
         B.Value := 0;
         B := B.Next;
      end loop;
