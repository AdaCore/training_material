-----------------------
Auto-active Proof Lab
-----------------------

- Find the :filename:`13_autoactive_proof` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- Define variable :code:`SPARKLIB_OBJECT_DIR` to have value :code:`obj` in the environment

  - For example with bash/zsh: :command:`export SPARKLIB_OBJECT_DIR=obj`

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

----------------
Selection Sort
----------------

- Find and open the files :filename:`sort_types.ads`, :filename:`sort.ads` and :filename:`sort.adb` in :toolname:`GNAT Studio`

  + Study the specification of procedure :ada:`Selection_Sort`. Is it a full
    functional specification?

  + Study the implementation of procedure :ada:`Selection_Sort`. Does it
    implement selection sort algorithm?

- Add a full functional contract to procedure :ada:`Swap`

  + Prove procedure :ada:`Swap`

- Add a full functional contract to procedure :ada:`Index_Of_Minimum`

  + Prove procedure :ada:`Index_Of_Minimum`

- Start by proving that :ada:`Values` is sorted when returning from procedure
  :ada:`Selection_Sort`

  + Add a loop invariant to procedure :ada:`Selection_Sort`

- Then prove that the output value of :ada:`Values` is a permutation of its input value

  + Hint: you need to update global ghost variable :ada:`Permutation`

- Run :toolname:`GNATprove` to prove the file

-----------------------------
Selection Sort - Variations
-----------------------------

- Find the :filename:`13_autoactive_proof` sub-directory in :filename:`answers`

  + It contains two sub-directories :filename:`answer1` and :filename:`answer2`

- In directory :filename:`answer1`, open the project :filename:`lab.gpr` in
  :toolname:`GNAT Studio`

  + This is a solution following the specification you worked on. Study it.
  + Run :toolname:`GNATprove` to prove the file

- In directory :filename:`answer2`, open the project :filename:`lab.gpr` in
  :toolname:`GNAT Studio`

  + This is another solution following a different specification for
    permutations. It uses multisets from the SPARK Library. Study it.
  + Run :toolname:`GNATprove` to prove the file

- Compare the two solutions

  + Which specification is more readable to you?
  + Which proof is easier for you?

------------------
Further Readings
------------------

- The second solution is based on the example in subsection "A Concrete
  Example: a Sort Algorithm" of section 7.9.3.2 of the SPARK User's Guide on
  "Manual Proof Using User Lemmas".

  + Read it and discuss with the course instructor.

- The blog post
  :url:`https://blog.adacore.com/i-cant-believe-that-i-can-prove-that-it-can-sort`
  presents 18 useful tips in the context of the proof of another sorting algorithm.

  + Read it and discuss with the course instructor.
