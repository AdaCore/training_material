=====
Lab
=====

--------------------
Advanced Proof Lab
--------------------

- Find the :filename:`010_advanced_proof` sub-directory in :filename:`source`

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

---------------------------
Array Initialization Loop
---------------------------

.. container:: animate 1-

   1. Find and open the files :filename:`loop_init.ads` and :filename:`loop_init.adb` in :toolname:`GNAT Studio`

   2. Run :toolname:`GNATprove` to prove the subprogram :ada:`Init_Table`

     + Can you explain why :ada:`Init_Table` is proved?

.. container:: animate 2-

   3. Loop is unrolled because it's size is small

      + You can see that by turning on :menu:`Output info messages` switch in the dialog

.. container:: animate 3-

   4. Change the type :ada:`Table` to be an unconstrained array

   .. code:: ada

      type Table is array (Index range <>) of Integer;

   5. Run :toolname:`GNATprove` to prove the subprogram :ada:`Init_Table`

     + Prover cannot prove the postcondition. Why?

.. container:: animate 4-

   6. Loop cannot be unrolled because size is unknown

------------------------
Helping Prove the Loop
------------------------

.. container:: animate 1-

   1. Add a loop invariant in :ada:`Init_Table`.

      + Hint: take inspiration in the postcondition.

.. container:: animate 2-

   .. code:: Ada

      pragma Loop_Invariant (for all K in T'First .. J => T(K) = 0);                                  

   2. Postcondition :ada:`Init_Table` now proves but ...

      + Prover still not sure about initialization of the object

.. container:: animate 3-

   3. First you need to *relax* the initialization requirement for **T**

.. container:: animate 4-

   .. code:: Ada

      procedure Init_Table (T : out Table)
      with
        Relaxed_Initialization => T,
        Post => (for all J in T'Range => T(J) = 0);

   4. Then you need to add a loop invariant to prove initialization

.. container:: animate 5-

   .. code:: Ada

      pragma Loop_Invariant
         (for all K in T'First .. J => T(K)'Initialized);

   5. And now your subprogram will prove.

--------------------
Array Mapping Loop
--------------------

- Run :toolname:`GNATprove` to prove the subprogram :ada:`Bump_Table`

- Add a loop invariant in :ada:`Bump_Table`.

   + Hint: use attribute :ada:`Loop_Entry`
   + Can you prove the subprogram without a loop frame condition?

- Change the assignment inside the loop into :ada:`T(J + 0) := T (J) + 1;`

   + Can you still prove the subprogram without a loop frame condition?
   + Discuss this with the course instructor.
   + Complete the loop invariant with a frame condition to prove :ada:`Bump_Table`

------------------------
Formal Container Loops
------------------------

- Run :toolname:`GNATprove` to prove the subprogram :ada:`Init_Vector`

- Add a loop invariant in :ada:`Init_Vector`

   + Hint: you need to state that :ada:`V.Last_Index` is preserved

- Run :toolname:`GNATprove` to prove the subprogram :ada:`Init_List`

- Add a loop invariant in :ada:`Init_List`

   + Hint: the position of cursor :ada:`Cu` in :ada:`L` is :ada:`Positions (L).Get (Cu)`
   + Hint: the sequence of components for :ada:`L` is :ada:`Model (L)`
