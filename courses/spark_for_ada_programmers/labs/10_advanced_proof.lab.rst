--------------------
Advanced Proof Lab
--------------------

- Find the :filename:`10_advanced_proof` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- Define variable :code:`SPARKLIB_OBJECT_DIR` to have value :code:`obj` in the environment

  - For example with bash/zsh: :command:`export SPARKLIB_OBJECT_DIR=obj`

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

---------------------------
Array Initialization Loop
---------------------------

- Find and open the files :filename:`loop_init.ads` and :filename:`loop_init.adb` in :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` to prove the subprogram :ada:`Init_Table`

  + Can you explain why :ada:`Init_Table` is proved?
  + Confirm this by rerunning :toolname:`GNATprove` with switch :command:`--info`

- Change the type :ada:`Table` to be an unconstrained array:

  .. code:: ada

     type Table is array (Index range <>) of Integer;

- Run :toolname:`GNATprove` to prove the subprogram :ada:`Init_Table`

  + Can you explain why the postcondition is not proved?
  + Confirm this by rerunning :toolname:`GNATprove` with switch :command:`--info`

- Add a loop invariant in :ada:`Init_Table`.

  + Hint: take inspiration in the postcondition.
  + Subprogram :ada:`Init_Table` should be proved except for initialization checks.

- Mark parameter :ada:`T` as having relaxed initialization.

  + Rerun :toolname:`GNATprove`.
  + Add the necessary loop invariant to complete the proof of :ada:`Init_Table`.

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
  + Hint: the sequence of elements for :ada:`L` is :ada:`Model (L)`
