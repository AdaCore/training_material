-----------
Proof Lab
-----------

- Find the :filename:`6_proof` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

---------------------------
Absence of Runtime Errors
---------------------------

- Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`
- Study the code and see if you can predict what's wrong.

   + These examples illustrate the basic forms of proof in SPARK.

- Use :menu:`SPARK` |rightarrow| :menu:`Prove File...` to analyse the body of package `Basics`.
- Click on the "Locations" tab to see the messages organised by unit.
- Make sure you understand the check messages that :toolname:`GNATprove` produces.

  + Discuss these with the course instructor.

- Add preconditions to avoid runtime errors in subprograms

  + Hint: use function :code:`Value_Rec` for procedures :code:`Bump_Rec` and
    :code:`Bump_The_Rec`
  + The objective is to get no messages when running :toolname:`GNATprove`.

---------------------------------
Functional Specifications (1/2)
---------------------------------

- Add a postcondition to procedure :code:`Swap_The_Table` stating that the
  values at indexes :code:`I` and :code:`J` have been exchanged.

- Run proof. Make sure you understand the check messages that
  :toolname:`GNATprove` produces.

  + Study the generated contracts and make sure you understand them.

- Add a postcondition to procedure :code:`Swap_Table` stating that the
  values at indexes :code:`I` and :code:`J` have been exchanged.

- Run proof.

  + The postcondition on procedure :code:`Swap_The_Table` should be proved now.
  + Add a postcondition to procedure :code:`Swap` to complete the proof.

- Add similarly a postcondition to procedures :code:`Bump_The_Rec` and
  :code:`Bump_Rec` stating that the value of component :code:`A` or :code:`B`
  (depending on the value of the discriminant) has been incremented

  + Hint: use again function :code:`Value_Rec`

---------------------------------
Functional Specifications (2/2)
---------------------------------

- Add similarly a postcondition to procedures :code:`Init_The_Rec` and
  :code:`Init_Rec` stating that the value of component :code:`A` or :code:`B`
  (depending on the value of the discriminant) is 1.

- Add similarly a postcondition to procedures :code:`Init_The_Table` and
  :code:`Init_Table` stating that the value of the first and last elements
  are 1 and 2.

  + Hint: you may have to strengthen the precondition of :code:`Init_Table`.

- Rerun :toolname:`GNATprove` with checkbox :menu:`Report check proved` selected.

  + Review the info messages and make sure you understand them.

- Modify the code or contracts and check that :toolname:`GNATprove` detects
  mismatches between them. Make sure you understand the check messages that
  :toolname:`GNATprove` produces.
