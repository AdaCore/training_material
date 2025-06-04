=====
Lab
=====

--------------------
Advanced Proof Lab
--------------------

- Find the :filename:`100_advanced_proof` sub-directory in :filename:`source`

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

   3. Loop is unrolled because its size is small

      + You can see that by turning on the :menu:`Output info messages` switch in the dialog

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

   1. Add a loop invariant in :ada:`Init_Table`

      + Hint: take inspiration in the postcondition

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

   5. And now your subprogram will prove!

--------------------
Array Mapping Loop
--------------------

.. container:: animate 1-

   1. Run :toolname:`GNATprove` to prove the subprogram :ada:`Bump_Table`

   ::

      loop_init.adb:14:24: info: cannot unroll loop (too many loop iterations)
      loop_init.ads:19:39: medium: postcondition might fail

.. container:: animate 2-

   2. Add a loop invariant in :ada:`Bump_Table`

      * Hint: use attribute :ada:`Loop_Entry`
      * Can you prove the subprogram without a loop frame condition?

.. container:: animate 3-

   3. No frame condition in this case

   .. code:: Ada

      pragma Loop_Invariant
         (for all K in T'First .. J => T(K) = T'Loop_Entry(K) + 1);

   4. Change the assignment inside the loop into the following, and try to prove: :ada:`T(J + 0) := T (J) + 1;` 

.. container:: animate 4-

   ::

      loop_init.adb:16:62: medium: loop invariant might not be preserved by an arbitrary iteration
      loop_init.adb:16:62: cannot prove T(K) = T'Loop_Entry(K) + 1

   5. We need to add a frame condition (things that haven't changed)

.. container:: animate 6-

   .. code:: Ada

      pragma Loop_Invariant
         (for all K in J .. T'Last =>
             (if K > J then T(K) = T'Loop_Entry(K)));
