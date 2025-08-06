=====
Lab
=====

-----------
Proof Lab
-----------

- Find the :filename:`060_proof` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-------------------------------
Understanding Run-time Errors
-------------------------------

.. container:: animate 1-

   - Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`
   - Study the code and see if you can predict what's "wrong"

      + These examples illustrate the basic forms of proof in SPARK

   - Use :menu:`SPARK` |rightarrow| :menu:`Prove File...` to analyze the body of package `Basics`
   - Click on the "Locations" tab to see the messages organized by unit
   - Make sure you understand the check messages that :toolname:`GNATprove` produces

.. container:: animate 2-

   :color-red:`basics.adb:14:24: medium: overflow check might fail`

   :color-red:`basics.adb:14:24: cannot prove upper bound for R.A + 1`

      Nothing prevents :ada:`R.A` from being :ada:`Integer'Last` which would cause a run-time error

   :color-red:`basics.adb:23:19: medium: array index check might fail`

   :color-red:`basics.adb:23:19: reason for check: value must be a valid index into the array`

      **T** is an unconstrained array, so there are no guarantees that **I** and **J** are valid

----------------------------
Absence of Run-time Errors
----------------------------

.. container:: animate 1-

- Add preconditions to avoid run-time errors in the subprograms

.. container:: animate 2-

   + Hint: use function :ada:`Value_Rec` for procedures :ada:`Bump_Rec` and :ada:`Bump_The_Rec`
   + The objective is to get no messages when running :toolname:`GNATprove`

.. container:: animate 3-

   .. code:: Ada

      procedure Bump_Rec (R : in out Rec)
      with
        Pre => Value_Rec (R) < Integer'Last;

      procedure Swap_Table (T : in out Table; I, J : Index)
      with
        Pre => I in T'Range and then J in T'Range;

      procedure Init_Table (T : out Table)
      with
        Pre => T'Length > 0;

      procedure Bump_The_Rec
      with
        Pre => Value_Rec (The_Rec) < Integer'Last;

------------------------
Proving the Code Works
------------------------

.. container:: animate 1-

   - Add a postcondition to procedure :ada:`Swap_The_Table` stating that the
     values at indexes :ada:`I` and :ada:`J` have been exchanged

.. container:: animate 2-

   .. code:: Ada

      procedure Swap_The_Table (I, J : Index)
      with
        Post => The_Table (I) = The_Table (J)'Old
          and then The_Table (J) = The_Table (I)'Old;

   - Run proof. What happens?

.. container:: animate 3-

   :color-red:`basics.ads:39:14: medium: postcondition might fail`

   :color-red:`basics.ads:39:14: cannot prove The_Table (I) = The_Table (J)'Old`

      The prover can't verify the result because it has no knowledge of the result for the call to :ada:`Swap_Table`

   - Add a postcondition to :ada:`Swap_Table` 

.. container:: animate 4-

   .. code:: Ada

      procedure Swap_Table (T : in out Table; I, J : Index)
      with
        Pre  => I in T'Range and then J in T'Range,
        Post => T (I) = T (J)'Old and then T (J) = T (I)'Old;

------------------------------------
Proving the Code Works (Continued)
------------------------------------

.. container:: animate 1-

   - Run proof. What happens now?

.. container:: animate 2-

   - :ada:`Swap_The_Table` now proves

      - Prover assumes a postcondition in a called subprogram is True

   - :ada:`Swap_Table` now fails to prove

      - Prover doesn't know anything about :ada:`Swap`

   - Add a postcondition for :ada:`Swap`

.. container:: animate 3-

   .. code:: Ada

      procedure Swap (X, Y : in out Integer)
      with Post => X = Y'Old and then Y = X'Old;

---------------------------
Functional Specifications 
---------------------------

- In the time left, add postconditions to the remaining subprograms

- Some hints

   - :ada:`Init_Table` precondition is insufficient
   - :ada:`Value_Rec` is easier to use than always checking the discriminant value
   - Running the prover with checkbox :menu:`Report checks proved` selected shows which subprograms have proven postconditions

- Full answers can be found in the course material
