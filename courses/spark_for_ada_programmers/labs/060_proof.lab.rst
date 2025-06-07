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
   - Study the code and see if you can predict what's "wrong".

      + These examples illustrate the basic forms of proof in SPARK.

   - Use :menu:`SPARK` |rightarrow| :menu:`Prove File...` to analyse the body of package `Basics`.
   - Click on the "Locations" tab to see the messages organized by unit.
   - Make sure you understand the check messages that :toolname:`GNATprove` produces.

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

- Add preconditions to avoid runtime errors in the subprograms

.. container:: animate 2-

   + Hint: use function :ada:`Value_Rec` for procedures :ada:`Bump_Rec` and :ada:`Bump_The_Rec`
   + The objective is to get no messages when running :toolname:`GNATprove`.

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
        Pre => T'Length >= 2;

      procedure Bump_The_Rec
      with
        Pre => Value_Rec (The_Rec) < Integer'Last;

---------------------------------
Functional Specifications (1/2)
---------------------------------

- Add a postcondition to procedure :ada:`Swap_The_Table` stating that the
  values at indexes :ada:`I` and :ada:`J` have been exchanged.

- Run proof. Make sure you understand the check messages that
  :toolname:`GNATprove` produces.

   + Study the generated contracts and make sure you understand them.

- Add a postcondition to procedure :ada:`Swap_Table` stating that the
  values at indexes :ada:`I` and :ada:`J` have been exchanged.

- Run proof.

   + The postcondition on procedure :ada:`Swap_The_Table` should be proved now.
   + Add a postcondition to procedure :ada:`Swap` to complete the proof.

- Add similarly a postcondition to procedures :ada:`Bump_The_Rec` and
  :ada:`Bump_Rec` stating that the value of component :ada:`A` or :ada:`B`
  (depending on the value of the discriminant) has been incremented

   + Hint: use again function :ada:`Value_Rec`

---------------------------------
Functional Specifications (2/2)
---------------------------------

- Add similarly a postcondition to procedures :ada:`Init_The_Rec` and
  :ada:`Init_Rec` stating that the value of component :ada:`A` or :ada:`B`
  (depending on the value of the discriminant) is 1.

- Add similarly a postcondition to procedures :ada:`Init_The_Table` and
  :ada:`Init_Table` stating that the value of the first and last components
  are 1 and 2.

   + Hint: you may have to strengthen the precondition of :ada:`Init_Table`.

- Rerun :toolname:`GNATprove` with checkbox :menu:`Report check proved` selected.

   + Review the info messages and make sure you understand them.

- Modify the code or contracts and check that :toolname:`GNATprove` detects
  mismatches between them. Make sure you understand the check messages that
  :toolname:`GNATprove` produces.
