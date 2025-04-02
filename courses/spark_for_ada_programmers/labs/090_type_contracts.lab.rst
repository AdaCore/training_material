=====
Lab
=====

--------------------
Type Contracts Lab
--------------------

- Find the :filename:`090_type_contracts` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

-----------------
Type Predicates
-----------------

- Find and open the files :filename:`basics.ads` and :filename:`basics.adb` in :toolname:`GNAT Studio`

- Run :toolname:`GNATprove` to prove the unit

   + Look at unproved predicate checks, can you explain them?
   + Does it make a difference that :ada:`Swap_Pair` is public and
     :ada:`Bump_Pair` is private?

- Fix the predicate check failure in :ada:`Bump_Pair`

   + Hint: use an aggregate assignment

- Fix the predicate check failure in :ada:`Swap_Pair` by using a base type
  without predicate for :ada:`Pair`

-----------------
Type Invariants
-----------------

- Run :toolname:`GNATprove` to prove the unit

   + Look at unproved invariant checks, can you explain them?
   + Does it make a difference that :ada:`Swap_Triplet` is public and
     :ada:`Bump_Triplet` is private?

- Fix the invariant check failure on the default value for :ada:`Triplet`

- Fix the invariant check failure in :ada:`Swap_Triplet`

   + Hint: the intent is for the value of all components to rotate

- Fix the invariant check failure in :ada:`Bump_And_Swap_Triplet`

   + Hint: look also at :ada:`Bump_Triplet`
   + Hint: you will need to add a postcondition to :ada:`Bump_Triplet`

--------------
All Together
--------------

- Run :toolname:`GNATprove` to prove the unit and display all proved checks

- Can you explain the presence of predicate checks and invariant checks?

   + How about the absence of checks in :ada:`Bump_And_Swap_Pair`?
   + How about the checks in :ada:`Bump_And_Swap_Triplet`?
